package com.heatonresearch.aifh.learning;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 7/30/13
 * Time: 3:28 PM
 * To change this template use File | Settings | File Templates.
 */
public interface ClassificationAlgorithm extends MachineLearningAlgorithm {
    int computeClassification(double[] input);
}
