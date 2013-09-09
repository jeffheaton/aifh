/*
 * Artificial Intelligence for Humans
 * Volume 1: Fundamental Algorithms
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh

 * Copyright 2013 by Jeff Heaton
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * For more information on Heaton Research copyrights, licenses
 * and trademarks visit:
 * http://www.heatonresearch.com/copyright
 */

package com.heatonresearch.aifh.discrete;

import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

/**
 * Perform discrete simulated annealing.  Discrete simulated annealing involves a problem with
 * a finite number of positions (or potential solutions).
 */
public abstract class DiscreteAnneal {
    /**
     * The random number generator.
     */
    private final GenerateRandom rnd = new MersenneTwisterGenerateRandom();

    /**
     * The current global best score.  The global best score is the best score that has been found over all of the
     * iterations.
     */
    private double globalBestScore = Double.POSITIVE_INFINITY;

    /**
     * The current score.
     */
    private double currentScore;

    /**
     * The maximum number of iterations to try.
     */
    private final int kMax;

    /**
     * The current iteration.
     */
    private int k;

    /**
     * The starting temperature.
     */
    private final double startingTemperature;

    /**
     * The ending temperature.
     */
    private final double endingTemperature;

    /**
     * The current temperature.
     */
    private double currentTemperature;

    /**
     * The number of cycles to try at each temperature.
     */
    private int cycles = 100;

    /**
     * The last probability of accepting a new non-improving move.
     */
    private double lastProbability;

    /**
     * Construct the Simulated Annealing trainer.
     *
     * @param theKMax                The maximum number of iterations.
     * @param theStartingTemperature The starting temperature.
     * @param theEndingTemperature   The ending temperature.
     */
    public DiscreteAnneal(final int theKMax, final double theStartingTemperature, final double theEndingTemperature) {
        this.kMax = theKMax;
        this.startingTemperature = theStartingTemperature;
        this.endingTemperature = theEndingTemperature;
    }

    /**
     * @return The correct temperature for the current iteration.
     */
    public double coolingSchedule() {
        final double ex = (double) k / (double) kMax;
        return this.startingTemperature * Math.pow(this.endingTemperature / this.startingTemperature, ex);
    }

    /**
     * Perform one training iteration.  This will execute the specified number of cycles at the current
     * temperature.
     */
    public void iteration() {

        // Is this the first time through, if so, then setup.
        if (k == 0) {
            this.currentScore = evaluate();
            foundNewBest();
            this.globalBestScore = this.currentScore;
        }

        // incrament the current iteration counter
        k++;

        // obtain the correct temperature
        this.currentTemperature = coolingSchedule();

        // perform the specified number of cycles
        for (int cycle = 0; cycle < this.cycles; cycle++) {
            // backup current state
            backupState();

            // randomize the method
            moveToNeighbor();

            // did we improve it?  Only keep the new method if it improved (greedy).
            final double trialScore = evaluate();

            // was this iteration an improvement?  If so, always keep.
            boolean keep = false;

            if (trialScore < this.currentScore) {
                // it was better, so always keep it
                keep = true;
            } else {
                // it was worse, so we might keep it
                this.lastProbability = calcProbability(currentScore, trialScore, this.currentTemperature);
                if (this.lastProbability > this.rnd.nextDouble()) {
                    keep = true;
                }
            }

            // should we keep this position?
            if (keep) {
                this.currentScore = trialScore;
                // better than global error
                if (trialScore < this.globalBestScore) {
                    this.globalBestScore = trialScore;
                    foundNewBest();
                }
            } else {
                // do not keep this position
                restoreState();
            }
        }
    }

    /**
     * Backup the current position (or state).
     */
    public abstract void backupState();

    /**
     * Restore the current position (or state).
     */
    public abstract void restoreState();

    /**
     * Handle the fact that we found a new global best.
     */
    public abstract void foundNewBest();

    /**
     * Move to a neighbor position.
     */
    public abstract void moveToNeighbor();

    /**
     * Evaluate the current position.
     *
     * @return The score.
     */
    public abstract double evaluate();

    /**
     * @return True, if training has reached the last iteration.
     */
    public boolean done() {
        return k >= kMax;
    }

    /**
     * @return The best score found so far.
     */
    public double getBestScore() {
        return this.globalBestScore;
    }

    /**
     * Calculate the probability that a worse solution will be accepted.  The higher the temperature the more likely
     * this will happen.
     *
     * @param ecurrent The current energy (or score/error).
     * @param enew     The new energy (or score/error).
     * @param t        The current temperature.
     * @return The probability of accepting a worse solution.
     */
    public double calcProbability(final double ecurrent, final double enew, final double t) {
        return Math.exp(-(Math.abs(enew - ecurrent) / t));
    }

    /**
     * @return The current iteration.
     */
    public int getK() {
        return this.k;
    }

    /**
     * @return The number of cycles per iteration.
     */
    public int getCycles() {
        return cycles;
    }

    /**
     * Set the number of cycles.
     *
     * @param cycles The number of cycles per iteration.
     */
    public void setCycles(final int cycles) {
        this.cycles = cycles;
    }

    /**
     * @return Returns the current status of the algorithm.
     */
    public String getStatus() {
        final StringBuilder result = new StringBuilder();
        result.append("k=");
        result.append(this.k);
        result.append(",kMax=");
        result.append(this.kMax);
        result.append(",t=");
        result.append(this.currentTemperature);
        result.append(",prob=");
        result.append(this.lastProbability);
        return result.toString();
    }
}
