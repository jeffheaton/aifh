package com.heatonresearch.aifh.examples.learning;

import com.heatonresearch.aifh.general.VectorUtil;
import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.learning.LearningMethod;
import com.heatonresearch.aifh.learning.RegressionAlgorithm;
import com.heatonresearch.aifh.normalize.Equilateral;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
    public void performIterations(final LearningMethod train, final int maxIterations, final double targetScore, final boolean shouldMinimize) {
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
    public static void query(final RegressionAlgorithm alg, final List<BasicData> theTrainingData) {
        for (final BasicData data : theTrainingData) {
            final double[] output = alg.computeRegression(data.getInput());
            System.out.println(Arrays.toString(data.getInput()) + " -> " + Arrays.toString(output) + ", Ideal: " + Arrays.toString(data.getIdeal()));
        }
    }

    /**
     * Query a regression algorithm using equilateral encoding.
     *
     * @param alg             The algorithm being used.
     * @param theTrainingData The training data.
     * @param items           The category items classified.
     * @param high            The high value.
     * @param low             The low value.
     */
    public static void queryEquilateral(
            final RegressionAlgorithm alg,
            final List<BasicData> theTrainingData,
            final Map<String, Integer> items,
            final double high, final double low) {
        // first, we need to invert the items.  Right now it maps from category to index.  We need index to category.
        final Map<Integer, String> invMap = new HashMap<Integer,String>();
        for (final Map.Entry<String, Integer> entry : items.entrySet()) {
            invMap.put(entry.getValue(), entry.getKey());
        }

        // now we can query
        final Equilateral eq = new Equilateral(items.size(), high, low);
        for (final BasicData data : theTrainingData) {
            final double[] output = alg.computeRegression(data.getInput());
            final int idealIndex = eq.decode(data.getIdeal());
            final int actualIndex = eq.decode(output);
            System.out.println(Arrays.toString(data.getInput()) + " -> " + invMap.get(actualIndex)
                    + ", Ideal: " + invMap.get(idealIndex));
        }
    }

    /**
     * Query a regression algorithm using one-of-n encoding.
     *
     * @param alg             The algorithm being used.
     * @param theTrainingData The training data.
     * @param items           The category items classified.
     */
    public static void queryOneOfN(
            final RegressionAlgorithm alg,
            final List<BasicData> theTrainingData,
            final Map<String, Integer> items) {
        // first, we need to invert the items.  Right now it maps from category to index.  We need index to category.
        final Map<Integer, String> invMap = new HashMap<Integer,String>();
        for (final Map.Entry<String, Integer> entry : items.entrySet()) {
            invMap.put(entry.getValue(), entry.getKey());
        }

        // now we can query
        for (final BasicData data : theTrainingData) {
            final double[] output = alg.computeRegression(data.getInput());
            final int idealIndex = VectorUtil.maxIndex(data.getIdeal());
            final int actualIndex = VectorUtil.maxIndex(output);
            System.out.println(Arrays.toString(data.getInput()) + " -> " + invMap.get(actualIndex)
                    + ", Ideal: " + invMap.get(idealIndex));
        }
    }
}
