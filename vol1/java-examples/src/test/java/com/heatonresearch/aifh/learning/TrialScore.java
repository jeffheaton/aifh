package com.heatonresearch.aifh.learning;

import com.heatonresearch.aifh.learning.score.ScoreFunction;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 8/31/13
 * Time: 11:55 AM
 * To change this template use File | Settings | File Templates.
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
