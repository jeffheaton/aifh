package com.heatonresearch.aifh.learning;

import com.heatonresearch.aifh.learning.score.ScoreFunction;
import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

public class TrainGreedyRandom {
    private RegressionAlgorithm algorithm;
    private GenerateRandom rnd = new MersenneTwisterGenerateRandom();
    private double lastError = Double.POSITIVE_INFINITY;
    private ScoreFunction score;
    private double lowRange = -10;
    private double highRange = 10;

    public TrainGreedyRandom(RegressionAlgorithm theAlgorithm, ScoreFunction theScore) {
        this.algorithm = theAlgorithm;
        this.score = theScore;
    }

    public void iteration() {
        int len = this.algorithm.getLongTermMemory().length;

        // backup current state
        double[] oldState = new double[len];
        System.arraycopy(this.algorithm.getLongTermMemory(), 0, oldState, 0, len);

        // randomize the method

        for (int i = 0; i < len; i++) {
            double d = this.rnd.nextDouble(this.lowRange, this.highRange);
            this.algorithm.getLongTermMemory()[i] = this.rnd.nextDouble(this.lowRange, this.highRange);
        }

        // did we improve it?  Only keep the new method if it improved (greedy).
        double currentError = score.calculateScore(this.algorithm);

        if (currentError < this.lastError) {
            this.lastError = currentError;
        } else {
            System.arraycopy(oldState, 0, this.algorithm.getLongTermMemory(), 0, len);
        }
    }

    public double getLastError() {
        return this.lastError;
    }

    public double getLowRange() {
        return lowRange;
    }

    public void setLowRange(final double lowRange) {
        this.lowRange = lowRange;
    }

    public double getHighRange() {
        return highRange;
    }

    public void setHighRange(final double highRange) {
        this.highRange = highRange;
    }
}
