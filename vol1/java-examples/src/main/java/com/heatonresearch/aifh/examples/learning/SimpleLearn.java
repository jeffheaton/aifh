package com.heatonresearch.aifh.examples.learning;

import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.learning.RBFNetwork;
import com.heatonresearch.aifh.learning.TrainGreedyRandom;

import java.util.Arrays;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 7/30/13
 * Time: 5:33 AM
 * To change this template use File | Settings | File Templates.
 */
public class SimpleLearn {

    public void performIterations(TrainGreedyRandom train, int maxIterations, double targetScore, boolean shouldMinimize) {
        int iterationNumber = 0;
        boolean done = false;

        do {
            iterationNumber++;

            train.iteration();

            if (iterationNumber >= maxIterations) {
                done = true;
            } else if (shouldMinimize && train.getLastError() < targetScore) {
                done = true;
            } else if (!shouldMinimize && train.getLastError() > targetScore) {
                done = true;
            }

            System.out.println("Iteration #" + iterationNumber + ", Score=" + train.getLastError());
        } while (!done);

        System.out.println("Final score: " + train.getLastError());
    }

    public static void query(RBFNetwork network, List<BasicData> theTrainingData) {
        for (int row = 0; row < theTrainingData.size(); row++) {
            BasicData data = theTrainingData.get(row);
            double[] output = network.computeRegression(data.getInput());
            System.out.println(Arrays.toString(data.getInput()) + " -> " + Arrays.toString(output));
        }
    }
}
