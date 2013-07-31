package com.heatonresearch.aifh.learning.score;

import com.heatonresearch.aifh.learning.MachineLearningAlgorithm;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 7/30/13
 * Time: 2:12 PM
 * To change this template use File | Settings | File Templates.
 */
public interface ScoreFunction {
    double calculateScore(MachineLearningAlgorithm algo);
}
