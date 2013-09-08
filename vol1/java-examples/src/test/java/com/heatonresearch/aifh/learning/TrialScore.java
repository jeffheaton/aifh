package com.heatonresearch.aifh.learning;

import com.heatonresearch.aifh.learning.score.ScoreFunction;

/**
 * A trial score function.
 */
public class TrialScore implements ScoreFunction {
    @Override
    public double calculateScore(final MachineLearningAlgorithm algo) {
        double sum = 0;
        for (int i = 0; i < algo.getLongTermMemory().length; i++) {
            sum += algo.getLongTermMemory()[i];
        }
        sum /= algo.getLongTermMemory().length;
        return Math.abs(10 - sum);
    }
}
