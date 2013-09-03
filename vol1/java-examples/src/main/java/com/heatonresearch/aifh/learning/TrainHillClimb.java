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
    private final MachineLearningAlgorithm algorithm;
    private GenerateRandom rnd = new MersenneTwisterGenerateRandom();
    private double lastError = Double.POSITIVE_INFINITY;
    private final ScoreFunction score;
    private final double[] candidate = new double[5];
    private final double[] stepSize;
    private final boolean shouldMinimize;

    public TrainHillClimb(boolean theShouldMinimize, MachineLearningAlgorithm theAlgorithm, ScoreFunction theScore, double acceleration, double stepSize) {
        this.algorithm = theAlgorithm;
        this.score = theScore;
        this.shouldMinimize = theShouldMinimize;

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

    public TrainHillClimb(boolean theShouldMinimize, MachineLearningAlgorithm theAlgorithm, ScoreFunction theScore) {
        this(theShouldMinimize, theAlgorithm, theScore, 1.2, 1);
    }

    @Override
    public void iteration() {
        int len = this.algorithm.getLongTermMemory().length;

        for (int i = 0; i < len; i++) {
            int best = -1;
            double bestScore = this.shouldMinimize ? Double.POSITIVE_INFINITY : Double.NEGATIVE_INFINITY;

            for (int j = 0; j < candidate.length; j++) {
                this.algorithm.getLongTermMemory()[i] += stepSize[i] * candidate[j];
                double temp = score.calculateScore(this.algorithm);
                this.algorithm.getLongTermMemory()[i] -= stepSize[i] * candidate[j];

                if ((temp < bestScore) ? shouldMinimize : !shouldMinimize) {

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

    @Override
    public void finishTraining() {

    }
}
