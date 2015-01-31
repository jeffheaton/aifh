/*
 * Artificial Intelligence for Humans
 * Volume 2: Nature Inspired Algorithms
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh
 *
 * Copyright 2014 by Jeff Heaton
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
package com.heatonresearch.aifh.aco;

import com.heatonresearch.aifh.AIFH;
import com.heatonresearch.aifh.learning.LearningMethod;
import com.heatonresearch.aifh.learning.MLMethod;
import com.heatonresearch.aifh.learning.score.ScoreFunction;
import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

import java.util.Arrays;

/**
 * This class implements continuous ant colony optimization (CACO)
 * <p/>
 * References:
 * <p/>
 * Training Neural Networks with Ant Colony Optimization,
 * Arun Pandian, Spring, 2013
 * <p/>
 * Krzysztof Socha and Christian Blum. “An ant colony optimization algorithm for
 * continuous optimization: application to feed-forward neural network training”, in
 * Springer London (2007).
 * <p/>
 * M.Dorigo, V.Maniezzo, and A.Colorni. “Ant System: Optimization by a colony of
 * cooperating agents”, in IEEE Transactions on Systems, Man, and Cybernetics,
 * 1996.
 */
public class ContinuousACO implements LearningMethod {
    /**
     * Sigma constant. Minimum standard deviation.
     */
    public static final double CONST_SIGMA = 0.1;

    /**
     * Q constant.  Weighting exponent factor.
     */
    public static final double CONST_Q = 0.08;

    /**
     * The population of ants.
     */
    private final ContinuousAnt[] population;

    /**
     * The population size.
     */
    private final int populationSize;

    /**
     * The parameter count.
     */
    private int paramCount = 0;

    /**
     * The weighting of each ant.
     */
    private final double[] weighting;

    /**
     * The sum of the weighting.
     */
    private double sumWeighting = 0;


    /**
     * Epsilon, learning rate.
     */
    private double epsilon = .75;

    /**
     * Random number generation.
     */
    private GenerateRandom random;

    /**
     * The algorithm that we are fitting.
     */
    private MLMethod algorithm;

    /**
     * The score function.
     */
    private ScoreFunction score;

    /**
     * The constructor.
     *
     * @param theAlgorithm      The algorithm to fit.
     * @param theScore          The score function.
     * @param thePopulationSize The population size.
     */
    public ContinuousACO(final MLMethod theAlgorithm, final ScoreFunction theScore, final int thePopulationSize) {
        this.algorithm = theAlgorithm;
        this.populationSize = thePopulationSize;
        this.score = theScore;
        this.random = new MersenneTwisterGenerateRandom();
        this.paramCount = theAlgorithm.getLongTermMemory().length;

        this.population = new ContinuousAnt[thePopulationSize * 2];
        this.weighting = new double[thePopulationSize];
        for (int i = 0; i < this.population.length; i++) {
            this.population[i] = new ContinuousAnt(paramCount, score.shouldMinimize());
            for (int j = 0; j < paramCount; j++) {
                this.population[i].getParams()[j] = random.nextDouble(-1, 1);
            }
        }

        updateScore();
        Arrays.sort(this.population);
        computeWeighting();
        sampleSolutions();
        Arrays.sort(this.population);

    }

    /**
     * Update the score.
     */
    private void updateScore() {

        for (final ContinuousAnt aPopulation : this.population) {
            System.arraycopy(aPopulation.getParams(), 0, this.algorithm.getLongTermMemory(), 0, this.paramCount);
            aPopulation.setScore(this.score.calculateScore(this.algorithm));
        }
    }

    /**
     * Compute the weighting for each ant.
     */
    private void computeWeighting() {
        sumWeighting = 0;
        double coef = (1 / (0.1 * Math.sqrt(2 * Math.PI)));
        for (int i = 0; i < this.populationSize; i++) {
            double exponent = (i * i) / (2 * CONST_Q * CONST_Q * this.populationSize * this.populationSize);
            this.weighting[i] = coef * Math.exp(-exponent);
            sumWeighting += weighting[i];
        }
    }

    /**
     * Compute the standard deviation.
     *
     * @param x The parameter to compute for.
     * @param l The population member.
     * @return The standard deviation.
     */
    private double computeSD(int x, int l) {
        double sum = 0.0;
        for (int i = 0; i < this.populationSize; i++) {
            sum += Math.abs(this.population[i].getParams()[x] - this.population[l].getParams()[x]) / (this.populationSize - 1);
        }
        if (sum < AIFH.DEFAULT_PRECISION) {
            return CONST_SIGMA;
        }
        return (epsilon * sum);
    }

    /**
     * Select a probability distribution function (PDF).
     *
     * @return The PDF index.
     */
    private int selectPDF() {
        int l = 0;
        double temp = 0;

        double r = random.nextDouble();
        for (int i = 0; i < this.populationSize; i++) {
            temp += weighting[i] / sumWeighting;
            if (r < temp) {
                l = i;
                break;
            }
        }
        return l;
    }

    /**
     * Sample new parameters.
     */
    private void sampleSolutions() {
        for (int i = this.populationSize; i < this.population.length; i++) {
            int pdf = selectPDF();
            for (int j = 0; j < paramCount; j++) {
                double sigma = computeSD(j, pdf);
                double mu = this.population[pdf].getParams()[j];
                double d = (random.nextGaussian() * sigma) + mu;
                this.population[i].getParams()[j] = d;
            }
        }
    }

    /**
     * @return The value for epsilon, the learning rate.
     */
    public double getEpsilon() {
        return epsilon;
    }

    /**
     * Set epsilon, the learning rate.
     *
     * @param epsilon The epsilon value.
     */
    public void setEpsilon(final double epsilon) {
        this.epsilon = epsilon;
    }

    /**
     * @return Random number generator.
     */
    public GenerateRandom getRandom() {
        return random;
    }

    public void setRandom(final GenerateRandom random) {
        this.random = random;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void iteration() {
        computeWeighting();
        sampleSolutions();
        updateScore();
        Arrays.sort(this.population);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double getLastError() {
        return this.population[0].getScore();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean done() {
        return false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getStatus() {
        return "";
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void finishTraining() {
        System.arraycopy(this.population[0].getParams(), 0, this.algorithm.getLongTermMemory(), 0, this.algorithm.getLongTermMemory().length);
    }
}
