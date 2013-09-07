package com.heatonresearch.aifh.learning;

/**
 * A classification algorithm is used to classify input data into classes.
 */
public interface ClassificationAlgorithm extends MachineLearningAlgorithm {
    /**
     * Classify the specified input into a group.
     *
     * @param input The input data.
     * @return The group the data was classified into.
     */
    int computeClassification(double[] input);
}
