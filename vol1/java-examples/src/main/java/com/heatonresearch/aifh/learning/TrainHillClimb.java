package com.heatonresearch.aifh.learning;

import com.heatonresearch.aifh.learning.score.ScoreFunction;
import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 8/24/13
 * Time: 7:28 PM
 * To change this template use File | Settings | File Templates.
 */
public class TrainHillClimb implements LearningAlgorithm {
    private RegressionAlgorithm algorithm;
    private GenerateRandom rnd = new MersenneTwisterGenerateRandom();
    private double lastError = Double.POSITIVE_INFINITY;
    private ScoreFunction score;
    private double[] candidate = new double[5];
    private double[] stepSize;

    public TrainHillClimb(RegressionAlgorithm theAlgorithm, ScoreFunction theScore, double acceleration, double stepSize) {
        this.algorithm = theAlgorithm;
        this.score = theScore;

        this.stepSize = new double[theAlgorithm.getLongTermMemory().length];
        for (int i = 0; i < theAlgorithm.getLongTermMemory().length; i++) {
            this.stepSize[i] = stepSize;
        }

        candidate[0] = -acceleration;
        candidate[1] = -1 / acceleration;
        candidate[2] = 0;
        candidate[3] = 1 / acceleration;
        candidate[4] = acceleration;
    }

    public TrainHillClimb(RegressionAlgorithm theAlgorithm, ScoreFunction theScore) {
        this(theAlgorithm, theScore, 1.2, 1);
    }

    @Override
    public void iteration() {
        int len = this.algorithm.getLongTermMemory().length;


        double before = score.calculateScore(this.algorithm);

        for (int i = 0; i < len; i++) {
            int best = -1;
            double bestScore = Double.POSITIVE_INFINITY;

            for (int j = 0; j < candidate.length; j++) {
                this.algorithm.getLongTermMemory()[i] += stepSize[i] * candidate[j];
                double temp = score.calculateScore(this.algorithm);
                this.algorithm.getLongTermMemory()[i] -= stepSize[i] * candidate[j];

                if (temp < bestScore) {
                    bestScore = temp;
                    this.lastError = bestScore;
                    best = j;
                }
            }

            if (best != -1) {
                this.algorithm.getLongTermMemory()[i] += stepSize[i] * candidate[best];
                stepSize[i] = stepSize[i] * candidate[best];
            }
        }
    }

    public boolean done() {
        return false;
    }

    @Override
    public double getLastError() {
        return this.lastError;
    }

    public String getStatus() {
        return "";
    }
}
