package com.heatonresearch.aifh.learning;

import com.heatonresearch.aifh.general.fns.FnRBF;
import com.heatonresearch.aifh.general.fns.GaussianFunction;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 7/29/13
 * Time: 12:57 PM
 * To change this template use File | Settings | File Templates.
 */
public class RBFNetwork implements RegressionAlgorithm, ClassificationAlgorithm {
    private int inputCount;
    private int outputCount;
    private FnRBF rbf[];
    private double[] longTermMemory;
    private int indexInputWeights;
    private int indexOutputWeights;
    private int indexRBFParams;

    public RBFNetwork(int theInputCount, int rbfCount, int theOutputCount) {

        this.inputCount = theInputCount;
        this.outputCount = theOutputCount;

        int inputWeightCount = inputCount * rbfCount;
        int outputWeightCount = rbfCount * outputCount;
        int rbfParams = (inputCount + 1) * rbfCount;
        this.longTermMemory = new double[
                inputWeightCount + outputWeightCount + rbfParams];

        this.indexInputWeights = 0;
        this.indexRBFParams = inputWeightCount;
        this.indexOutputWeights = indexRBFParams + rbfParams;

        this.rbf = new FnRBF[rbfCount];

        for (int i = 0; i < rbfCount; i++) {
            int rbfIndex = this.indexRBFParams + ((inputCount + 1) * i);
            this.rbf[i] = new GaussianFunction(inputCount, this.longTermMemory, rbfIndex);
        }
    }

    public double[] computeRegression(double[] input) {

        // first, compute the output values of each of the RBFs
        double[] rbfOutput = new double[rbf.length];

        for (int rbfIndex = 0; rbfIndex < rbfOutput.length; rbfIndex++) {

            // weight the input
            double[] weightedInput = new double[input.length];

            for (int inputIndex = 0; inputIndex < input.length; inputIndex++) {
                int memoryIndex = this.indexInputWeights + (rbfIndex * this.inputCount) + inputIndex;
                weightedInput[inputIndex] = input[inputIndex] * this.longTermMemory[memoryIndex];
            }

            // calculate the rbf
            rbfOutput[rbfIndex] = this.rbf[rbfIndex].evaluate(weightedInput);
        }

        // second, calculate the output, which is the result of the weighted result of the RBF's.
        double[] result = new double[this.outputCount];

        for (int outputIndex = 0; outputIndex < result.length; outputIndex++) {
            double sum = 0;
            for (int rbfIndex = 0; rbfIndex < rbfOutput.length; rbfIndex++) {
                int memoryIndex = this.indexOutputWeights + (outputIndex * rbf.length) + rbfIndex;
                sum += rbfOutput[rbfIndex] * this.longTermMemory[memoryIndex];
            }
            result[outputIndex] = sum;
        }

        // finally, return the result.
        return result;
    }

    public double[] getLongTermMemory() {
        return longTermMemory;
    }


    @Override
    public int computeClassification(final double[] input) {
        return 0;  //To change body of implemented methods use File | Settings | File Templates.
    }
}
