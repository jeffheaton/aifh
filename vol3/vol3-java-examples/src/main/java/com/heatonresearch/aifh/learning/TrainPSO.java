/*
 * Artificial Intelligence for Humans
 * Volume 3: Deep Learning and Neural Networks
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh
 *
 * Copyright 2014-2015 by Jeff Heaton
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

import com.heatonresearch.aifh.general.VectorAlgebra;
import com.heatonresearch.aifh.learning.score.ScoreFunction;
import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

/**
 * Iteratively trains a population by applying
 * particle swarm optimisation (PSO).
 * <p/>
 * Based on Encog contribution by:
 * Geoffroy Noel, https://github.com/goffer-looney
 * <p/>
 * References:
 * James Kennedy and Russell C. Eberhart, Particle swarm optimization,
 * Proceedings of the IEEE International Conference on Neural Networks,
 * 1995, pp. 1942-1948
 *
 * @author Geoffroy Noel
 */
public class TrainPSO implements LearningMethod {
    /**
     * The machine learning algorithm to optimize.
     */
    private final MLMethod[] particles;

    /**
     * The random number generator to use.
     */
    private final GenerateRandom rnd = new MersenneTwisterGenerateRandom();

    /**
     * The current error.
     */
    private double currentError;

    /**
     * The scoring function, this determines the energy (error) of the current solution.
     */
    private final ScoreFunction score;

    /**
     * Swarm state and memories.
     */
    protected double[][] velocities;
    protected double[][] bestVectors;
    protected double[] bestScores;
    protected int bestVectorIndex;

    /**
     * Although this is redundant with m_bestVectors[bestVectorIndex],
     * bestVectors[bestVectorIndex] is not thread safe.
     */
    private final double[] bestVector;

    // Determines the size of the search space.
    // The position components of particle will be bounded to
    // [-maxPos, maxPos]
    // A well chosen range can improve the performance.
    // -1 is a special value that represents boundless search space.

    /**
     *
     */
    protected double maxPosition = -1;

    // Maximum change one particle can take during one iteration.
    // Imposes a limit on the maximum absolute value of the velocity
    // components of a particle.
    // Affects the granularity of the search.
    // If too high, particle can fly past optimum solution.
    // If too low, particle can get stuck in local minima.
    // Usually set to a fraction of the dynamic range of the search
    // space (10% was shown to be good for high dimensional problems).
    // -1 is a special value that represents boundless velocities.
    protected double maxVelocity = 2;

    // c1, cognitive learning rate >= 0
    // tendency to return to personal best position
    protected double c1 = 2.0;
    // c2, social learning rate >= 0
    // tendency to move towards the swarm best position
    protected double c2 = 2.0;

    private double bestScore;

    // w, inertia weight.
    // Controls global (higher value) vs local exploration
    // of the search space.
    // Analogous to temperature in simulated annealing.
    // Must be chosen carefully or gradually decreased over time.
    // Value usually between 0 and 1.
    protected double inertiaWeight = 0.4;

    public TrainPSO(final MLMethod[] theParticles,
                    final ScoreFunction theCalculateScore) {
        this.particles = theParticles;
        this.score = theCalculateScore;
        int vectorSize = theParticles[0].getLongTermMemory().length;
        int particleCount = theParticles.length;

        this.bestVectors = new double[particleCount][vectorSize];
        this.velocities = new double[particleCount][vectorSize];
        this.bestScores = new double[particleCount];

        this.bestVectorIndex = -1;

        this.bestVector = new double[vectorSize];

        for (final double[] velocity : this.velocities) {
            VectorAlgebra.randomise(this.rnd, velocity, this.maxVelocity);
        }
    }

    /**
     * Update the velocity, position and personal
     * best position of a particle
     *
     * @param particleIndex index of the particle in the swarm
     */
    protected void updateParticle(int particleIndex) {

        double[] particlePosition = this.particles[particleIndex].getLongTermMemory();


        updateVelocity(particleIndex);

        // velocity clamping
        VectorAlgebra.clampComponents(this.velocities[particleIndex], this.maxVelocity);

        // new position (Xt = Xt-1 + Vt)
        VectorAlgebra.add(particlePosition, this.velocities[particleIndex]);

        // pin the particle against the boundary of the search space.
        // (only for the components exceeding maxPosition)
        VectorAlgebra.clampComponents(particlePosition, this.maxPosition);

        updatePersonalBestPosition(particleIndex, particlePosition);
    }

    /**
     * Update the velocity of a particle
     *
     * @param particleIndex index of the particle in the swarm
     */
    protected void updateVelocity(int particleIndex) {
        double[] particlePosition = this.particles[particleIndex].getLongTermMemory();
        double[] vtmp = new double[particlePosition.length];


        // Standard PSO formula

        // inertia weight
        VectorAlgebra.mul(this.velocities[particleIndex], this.inertiaWeight);

        // cognitive term
        VectorAlgebra.copy(vtmp, this.bestVectors[particleIndex]);
        VectorAlgebra.sub(vtmp, particlePosition);
        VectorAlgebra.mulRand(this.rnd, vtmp, this.c1);
        VectorAlgebra.add(this.velocities[particleIndex], vtmp);

        // social term
        if (particleIndex != this.bestVectorIndex) {
            VectorAlgebra.copy(vtmp, this.bestVector);
            VectorAlgebra.sub(vtmp, particlePosition);
            VectorAlgebra.mulRand(this.rnd, vtmp, this.c2);
            VectorAlgebra.add(this.velocities[particleIndex], vtmp);
        }
    }

    /**
     * Update the personal best position of a particle.
     *
     * @param particleIndex    index of the particle in the swarm
     * @param particlePosition the particle current position vector
     */
    protected void updatePersonalBestPosition(int particleIndex, double[] particlePosition) {
        // set the network weights and biases from the vector
        double score = this.score.calculateScore(this.particles[particleIndex]);

        // update the best vectors (g and i)
        if ((this.bestScores[particleIndex] == 0) || isScoreBetter(score, this.bestScores[particleIndex])) {
            this.bestScores[particleIndex] = score;
            VectorAlgebra.copy(this.bestVectors[particleIndex], particlePosition);
        }
    }

    /**
     * Update the swarm's best position
     */
    protected void updateGlobalBestPosition() {
        boolean bestUpdated = false;
        for (int i = 0; i < this.particles.length; i++) {
            if ((this.bestVectorIndex == -1) || isScoreBetter(this.bestScores[i], this.bestScores[this.bestVectorIndex])) {
                this.bestVectorIndex = i;
                bestUpdated = true;
            }
        }
        if (bestUpdated) {
            VectorAlgebra.copy(this.bestVector, this.bestVectors[this.bestVectorIndex]);
            this.bestScore = this.bestScores[this.bestVectorIndex];
        }
    }

    /**
     * Compares two scores.
     *
     * @param score1 a score
     * @param score2 a score
     * @return true if score1 is better than score2
     */
    boolean isScoreBetter(double score1, double score2) {
        return ((this.score.shouldMinimize() && (score1 < score2)) || ((!this.score.shouldMinimize()) && (score1 > score2)));
    }


    /**
     * {@inheritDoc}
     */
    @Override
    public void iteration() {
        for (int i = 0; i < this.particles.length; i++) {
            updateParticle(i);
        }

        updateGlobalBestPosition();
    }

    /**
     * @return False, this algorithm can be iterated an unlimited number of times.
     */
    @Override
    public boolean done() {
        return false;
    }

    /**
     * @return The error (or energy) from the last iteration.
     */
    @Override
    public double getLastError() {
        return this.bestScore;
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
     * Copy the global best solution to the machine learning algorithm.  It is very important to call this method.
     */
    @Override
    public void finishTraining() {

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getStatus() {
        return "";
    }

    public MLMethod getBestParticle() {
        VectorAlgebra.copy(this.particles[this.bestVectorIndex].getLongTermMemory(), this.bestVector);
        return this.particles[this.bestVectorIndex];
    }
}
