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

package com.heatonresearch.aifh.learning;

import com.heatonresearch.aifh.learning.score.ScoreFunction;
import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

/**
 * Train a Machine Learning Algorithm using Simulated Annealing.  Simulated Annealing is a Monte Carlo algorithm that
 * is based on annealing in metallurgy, a technique involving heating and controlled cooling of a material to increase
 * the size of its crystals and reduce their defects, both are attributes of the material that depend on its
 * thermodynamic free energy.
 * <p/>
 * The Simulated Annealing algorithm works by randomly changing a vector of doubles.  This is the long term memory of
 * the Machine Learning algorithm.  While this happens a temperature is slowly decreased.  When this temperature is
 * higher, the Simulated Annealing algorithm is more likely to accept changes that have a higher error (or energy)
 * than the current state.
 * <p/>
 * There are several important components to any Simulated Learning Algorithm:
 * <p/>
 * First, the randomization technique.  This is performed by the method performRandomize.  To randomize differently,
 * override this method.
 * <p/>
 * Secondly, the cooling schedule.  This determines how quickly the current temperature will fall.  This is controlled
 * by the coolingSchedule.  To define a different cooling schedule, override this method.
 * <p/>
 * Finally, the probability of accepting a higher-error (energy) solution.  This is defined by a Probability
 * Distribution Function (PDF) contained in calcProbability.  To define a different PDF, override this method.
 * <p/>
 * http://en.wikipedia.org/wiki/Simulated_annealing
 */
public class TrainAnneal implements LearningMethod {
    /**
     * The machine learning algorithm to optimize.
     */
    private final MachineLearningAlgorithm algorithm;

    /**
     * The random number generator to use.
     */
    private final GenerateRandom rnd = new MersenneTwisterGenerateRandom();

    /**
     * The current error of best solution ever found.
     */
    private double globalBestError = Double.POSITIVE_INFINITY;

    /**
     * The current best solution ever found.
     */
    private final double[] globalBest;

    /**
     * The current error.
     */
    private double currentError;

    /**
     * The scoring function, this determines the energy (error) of the current solution.
     */
    private final ScoreFunction score;

    /**
     * The maximum number of iterations.
     */
    private final int kMax;

    /**
     * The current iteration number.
     */
    private int k;

    /**
     * The starting temperature.
     */
    private final double startingTemperature;

    /**
     * The ending temperature.  Do not set to zero, as many cooling schedules asymptotically approach zero.
     * Rather, use something close to zero, like 0.0001.
     */
    private final double endingTemperature;

    /**
     * The current temperature.
     */
    private double currentTemperature;

    /**
     * The number of random moves to try for each iteration.
     */
    private int cycles = 100;

    /**
     * The probability for the last iteration cycle.
     */
    private double lastProbability;

    /**
     * Construct the simulated annealing trainer.  Use 1000 iterations and temperature from 400 to 0.0001.
     *
     * @param theAlgorithm The algorithm to optimize.
     * @param theScore     The score function.
     */
    public TrainAnneal(final MachineLearningAlgorithm theAlgorithm, final ScoreFunction theScore) {
        this(theAlgorithm, theScore, 100, 400, 0.0001);
    }

    /**
     * Construct the simulated annealing trainer.
     *
     * @param theAlgorithm           The algorithm to optimize.
     * @param theScore               The score function.
     * @param theKMax                The max number of iterations.
     * @param theStartingTemperature The starting temperature.
     * @param theEndingTemperature   The ending temperature.
     */
    public TrainAnneal(final MachineLearningAlgorithm theAlgorithm, final ScoreFunction theScore, final int theKMax, final double theStartingTemperature, final double theEndingTemperature) {
        this.algorithm = theAlgorithm;
        this.score = theScore;
        this.kMax = theKMax;
        this.currentError = score.calculateScore(this.algorithm);
        this.startingTemperature = theStartingTemperature;
        this.endingTemperature = theEndingTemperature;
        this.globalBest = new double[theAlgorithm.getLongTermMemory().length];
        System.arraycopy(this.algorithm.getLongTermMemory(), 0, this.globalBest, 0, this.globalBest.length);
    }

    /**
     * The cooling schedule.  This is a Probability Distribution Function (PDF) that specifies the probability,
     * at a given temperature, of accepting a higher-energy move.
     *
     * @return The probability.
     */
    public double coolingSchedule() {
        final double ex = (double) k / (double) kMax;
        return this.startingTemperature * Math.pow(this.endingTemperature / this.startingTemperature, ex);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void iteration() {
        final int len = this.algorithm.getLongTermMemory().length;
        k++;

        this.currentTemperature = coolingSchedule();

        for (int cycle = 0; cycle < this.cycles; cycle++) {
            // backup current state
            final double[] oldState = new double[len];
            System.arraycopy(this.algorithm.getLongTermMemory(), 0, oldState, 0, len);

            // randomize the method
            performRandomize(this.algorithm.getLongTermMemory());

            // did we improve it?
            final double trialError = score.calculateScore(this.algorithm);

            // was this iteration an improvement?  If so, always keep.
            boolean keep = false;

            if (trialError < this.currentError) {
                keep = true;
            } else {

                this.lastProbability = calcProbability(currentError, trialError, this.currentTemperature);
                if (this.lastProbability > this.rnd.nextDouble()) {
                    keep = true;
                }
            }

            if (keep) {
                this.currentError = trialError;
                // better than global error
                if (trialError < this.globalBestError) {
                    this.globalBestError = trialError;
                    System.arraycopy(this.algorithm.getLongTermMemory(), 0, oldState, 0, len);
                    System.arraycopy(this.algorithm.getLongTermMemory(), 0, this.globalBest, 0, len);
                }
            } else {
                System.arraycopy(oldState, 0, this.algorithm.getLongTermMemory(), 0, len);
            }
        }
    }

    /**
     * Randomly move to a new location.  To specify a new randomization function, override this method.
     *
     * @param memory The long term memory.
     */
    public void performRandomize(final double[] memory) {
        for (int i = 0; i < memory.length; i++) {
            final double d = this.rnd.nextGaussian() / 10;
            memory[i] += d;
        }
    }

    /**
     * @return True, if we have reached the max iterations.
     */
    @Override
    public boolean done() {
        return k >= kMax;
    }

    /**
     * @return The error (or energy) from the last iteration.
     */
    @Override
    public double getLastError() {
        return this.globalBestError;
    }

    /**
     * Calculate the probability that we will accept a move that takes us to a higher energy (higher error)
     * position.
     *
     * @param ecurrent The current energy.
     * @param enew     The new energy if we move.
     * @param t        The current temperature.
     * @return The probability.
     */
    public double calcProbability(final double ecurrent, final double enew, final double t) {
        return Math.exp(-(Math.abs(enew - ecurrent) / t));
    }

    /**
     * @return The current temperature.
     */
    public double getCurrentTemperature() {
        return currentTemperature;
    }

    /**
     * @return The current iteration number.
     */
    public int getK() {
        return k;
    }

    /**
     * @return The starting temperature.
     */
    public double getStartingTemperature() {
        return startingTemperature;
    }

    /**
     * @return The ending temperature.
     */
    public double getEndingTemperature() {
        return endingTemperature;
    }

    /**
     * @return The number of cycles per iteration.
     */
    public int getCycles() {
        return cycles;
    }

    /**
     * @return The last probability.
     */
    public double getLastProbability() {
        return lastProbability;
    }

    /**
     * Set the number of cycles per iteration.
     *
     * @param cycles The number of cycles per iteration.
     */
    public void setCycles(final int cycles) {
        this.cycles = cycles;
    }

    /**
     * Copy the global best solution to the machine learning algorithm.  It is very important to call this method.
     */
    @Override
    public void finishTraining() {
        System.arraycopy(this.globalBest, 0, this.algorithm.getLongTermMemory(), 0, this.globalBest.length);
    }

    /**
     * {@inheritDoc}
     */
    @Override
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
