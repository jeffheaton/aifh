package com.heatonresearch.aifh.learning;

/**
 * A regression algorithm provides an output for the given input.  This allows the machine learning algorithm to
 * approximate a function.
 */
public interface RegressionAlgorithm extends MachineLearningAlgorithm {
    /**
     * Compute the output for the specified input.
     *
     * @param input The input.
     * @return The regression output.
     */
    double[] computeRegression(double[] input);
}
