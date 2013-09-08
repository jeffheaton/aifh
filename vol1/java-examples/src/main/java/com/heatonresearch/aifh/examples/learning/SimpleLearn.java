package com.heatonresearch.aifh.examples.learning;

import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.learning.LearningAlgorithm;
import com.heatonresearch.aifh.learning.RegressionAlgorithm;

import java.util.Arrays;
import java.util.List;

/**
 * Base class for many of the iteration based examples.  It will loop over iterations and display stats.
 */
public class SimpleLearn {

    /**
     * Perform training iterations.
     *
     * @param train          The learning algorithm.
     * @param maxIterations  The max number of iterations.
     * @param targetScore    The target score.
     * @param shouldMinimize True, if we should minimize.
     */
    public void performIterations(LearningAlgorithm train, int maxIterations, double targetScore, boolean shouldMinimize) {
        int iterationNumber = 0;
        boolean done = false;

        do {
            iterationNumber++;

            train.iteration();

            if (train.done()) {
                done = true;
            } else if (iterationNumber >= maxIterations) {
                done = true;
            } else if (shouldMinimize && train.getLastError() < targetScore) {
                done = true;
            } else if (!shouldMinimize && train.getLastError() > targetScore) {
                done = true;
            }

            System.out.println("Iteration #" + iterationNumber + ", Score=" + train.getLastError() + ", " + train.getStatus());
        } while (!done);

        train.finishTraining();
        System.out.println("Final score: " + train.getLastError());
    }

    /**
     * Query a regression algorithm and see how close it matches the training data.
     *
     * @param alg             The algorithm to evaluate.
     * @param theTrainingData The training data.
     */
    public static void query(RegressionAlgorithm alg, List<BasicData> theTrainingData) {
        for (BasicData data : theTrainingData) {
            double[] output = alg.computeRegression(data.getInput());
            System.out.println(Arrays.toString(data.getInput()) + " -> " + Arrays.toString(output) + ", Ideal: " + Arrays.toString(data.getIdeal()));
        }
    }
}
