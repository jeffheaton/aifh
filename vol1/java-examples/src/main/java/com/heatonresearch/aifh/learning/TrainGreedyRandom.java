package com.heatonresearch.aifh.learning;

import com.heatonresearch.aifh.error.ErrorCalculation;
import com.heatonresearch.aifh.error.ErrorCalculationMSE;
import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

import java.util.Arrays;

public class TrainGreedyRandom {

    /**
     * The input necessary for XOR.
     */
    public static double XOR_INPUT[][] = {{0.0, 0.0}, {1.0, 0.0},
            {0.0, 1.0}, {1.0, 1.0}};

    /**
     * The ideal data necessary for XOR.
     */
    public static double XOR_IDEAL[][] = {{0.0}, {1.0}, {1.0}, {0.0}};

    private RBFNetwork method;
    private double[][] inputData;
    private double[][] idealData;
    private GenerateRandom rnd = new MersenneTwisterGenerateRandom();
    private ErrorCalculation errorCalc = new ErrorCalculationMSE();
    private double lastError = Double.POSITIVE_INFINITY;

    public TrainGreedyRandom(RBFNetwork theMethod, double[][] theInputData, double[][] theIdealData) {
        this.method = theMethod;
        this.inputData = theInputData;
        this.idealData = theIdealData;
    }

    public void iteration() {
        int len = this.method.getLongTermMemory().length;

        // backup current state
        double[] oldState = new double[len];
        System.arraycopy(this.method.getLongTermMemory(), 0, oldState, 0, len);

        // randomize the method

        for (int i = 0; i < len; i++) {
            this.method.getLongTermMemory()[i] = this.rnd.nextDouble(-10, 10);
        }

        // evaulate
        errorCalc.clear();
        for (int row = 0; row < this.inputData.length; row++) {
            double[] output = this.method.computeRegression(this.inputData[row]);
            errorCalc.updateError(output, this.idealData[row], 1.0);
        }

        // did we improve it?  Only keep the new method if it improved (greedy).
        double currentError = errorCalc.calculate();

        if (currentError < this.lastError) {
            this.lastError = currentError;
        } else {
            System.arraycopy(oldState, 0, this.method.getLongTermMemory(), 0, len);
        }
        System.out.println(this.lastError);
    }

    public static void query(RBFNetwork network, double[][] input, double[][] ideal) {
        for (int row = 0; row < input.length; row++) {
            double[] output = network.computeRegression(input[row]);
            System.out.println(Arrays.toString(input[row]) + " -> " + Arrays.toString(output));
        }
    }


    public static void main(String[] args) {
        RBFNetwork network = new RBFNetwork(2, 5, 1);
        TrainGreedyRandom train = new TrainGreedyRandom(network, XOR_INPUT, XOR_IDEAL);
        for (int i = 0; i < 100000; i++) {
            train.iteration();
        }
        query(network, XOR_INPUT, XOR_IDEAL);
    }
}
