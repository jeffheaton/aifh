package com.heatonresearch.aifh.learning;

/**
 * A learning algorithm is used to train a machine learning algorithm to better classify or perform regression on
 * input data.
 */
public interface LearningAlgorithm {

    /**
     * Perform one training iteration.
     */
    void iteration();

    /**
     * @return The error from the last training iteration.
     */
    double getLastError();

    /**
     * @return True, if we are done learning.  Not all learning algorithms know when they are done, in this case
     *         false is always returned.
     */
    boolean done();

    /**
     * @return A string that indicates the status of training.
     */
    String getStatus();

    /**
     * Should be called after the last iteration to make sure training completes any final tasks.
     */
    void finishTraining();
}
