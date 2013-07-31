package com.heatonresearch.aifh.learning;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 7/30/13
 * Time: 2:08 PM
 * To change this template use File | Settings | File Templates.
 */
public interface RegressionAlgorithm extends MachineLearningAlgorithm {
    double[] computeRegression(double[] input);
}
