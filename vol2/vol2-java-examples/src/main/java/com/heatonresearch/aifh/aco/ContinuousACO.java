package com.heatonresearch.aifh.aco;

import com.heatonresearch.aifh.AIFH;
import com.heatonresearch.aifh.learning.LearningMethod;
import com.heatonresearch.aifh.learning.MLMethod;
import com.heatonresearch.aifh.learning.score.ScoreFunction;
import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

import java.util.Arrays;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 5/5/14
 * Time: 3:14 PM
 * To change this template use File | Settings | File Templates.
 */
public class ContinuousACO implements LearningMethod {
    public static final double CONST_SIGMA = 0.1;
    public static final double CONST_Q = 0.08;

    private final ContACOElement[] population;
    private final int populationSize;
    private int paramCount = 0;
    private final double[] weighting;
    private double sumWeighting = 0;
    private double epsilon = .75;
    private GenerateRandom random;

    private MLMethod algorithm;
    private ScoreFunction score;

    public ContinuousACO(final MLMethod theAlgorithm, final ScoreFunction theScore, final int thePopulationSize) {
        this.algorithm = theAlgorithm;
        this.populationSize = thePopulationSize;
        this.score = theScore;
        this.random = new MersenneTwisterGenerateRandom();
        this.paramCount = theAlgorithm.getLongTermMemory().length;

        this.population = new ContACOElement[thePopulationSize * 2];
        this.weighting = new double[thePopulationSize];
        for (int i = 0; i < this.population.length; i++) {
            this.population[i] = new ContACOElement(paramCount, score.shouldMinimize());
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

    private void updateScore() {

        for (final ContACOElement aPopulation : this.population) {
            System.arraycopy(aPopulation.getParams(), 0, this.algorithm.getLongTermMemory(), 0, this.paramCount);
            aPopulation.setScore(this.score.calculateScore(this.algorithm));
        }
    }

    private void computeWeighting() {
        sumWeighting = 0;
        for (int i = 0; i < this.populationSize; i++) {
            double exponent = (i * i) / (2 * CONST_Q * CONST_Q * this.populationSize * this.populationSize);
            this.weighting[i] =
                    (1 / (0.1 * Math.sqrt(2 * Math.PI))) * Math.pow(Math.E, -exponent);
            sumWeighting += weighting[i];
        }
    }

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

    public double getEpsilon() {
        return epsilon;
    }

    public void setEpsilon(final double epsilon) {
        this.epsilon = epsilon;
    }

    public GenerateRandom getRandom() {
        return random;
    }

    public void setRandom(final GenerateRandom random) {
        this.random = random;
    }

    @Override
    public void iteration() {
        computeWeighting();
        sampleSolutions();
        updateScore();
        Arrays.sort(this.population);
    }

    @Override
    public double getLastError() {
        return this.population[0].getScore();
    }

    @Override
    public boolean done() {
        return false;
    }

    @Override
    public String getStatus() {
        return "";
    }

    @Override
    public void finishTraining() {

    }
}
