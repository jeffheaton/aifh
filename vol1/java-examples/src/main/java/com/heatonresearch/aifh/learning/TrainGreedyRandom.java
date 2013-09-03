package com.heatonresearch.aifh.learning;

import com.heatonresearch.aifh.learning.score.ScoreFunction;
import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

public class TrainGreedyRandom implements LearningAlgorithm {
    private final MachineLearningAlgorithm algorithm;
    private final GenerateRandom rnd = new MersenneTwisterGenerateRandom();
    private double lastError = Double.POSITIVE_INFINITY;
    private final ScoreFunction score;
    private double lowRange = -10;
    private double highRange = 10;
    private final boolean shouldMinimize;

    public TrainGreedyRandom(boolean theShouldMinimize, MachineLearningAlgorithm theAlgorithm, ScoreFunction theScore) {
        this.algorithm = theAlgorithm;
        this.score = theScore;
        this.shouldMinimize = theShouldMinimize;
    }

    @Override
    public void iteration() {
        int len = this.algorithm.getLongTermMemory().length;

        // backup current state
        double[] oldState = new double[len];
        System.arraycopy(this.algorithm.getLongTermMemory(), 0, oldState, 0, len);

        // randomize the method
        performRandomize(this.algorithm.getLongTermMemory());

        // did we improve it?  Only keep the new method if it improved (greedy).
        double currentError = score.calculateScore(this.algorithm);

        if ((currentError < this.lastError) ? shouldMinimize : !shouldMinimize) {
            this.lastError = currentError;
        } else {
            System.arraycopy(oldState, 0, this.algorithm.getLongTermMemory(), 0, len);
        }
    }

    public void performRandomize(double[] memory) {
        for (int i = 0; i < memory.length; i++) {
            memory[i] = this.rnd.nextDouble(this.lowRange, this.highRange);
        }
    }

    @Override
    public String getStatus() {
        return "";
    }

    @Override
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

    public boolean done() {
        return false;
    }

    @Override
    public void finishTraining() {

    }
}
