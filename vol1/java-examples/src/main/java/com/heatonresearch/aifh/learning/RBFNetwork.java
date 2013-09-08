package com.heatonresearch.aifh.learning;

import com.heatonresearch.aifh.general.VectorUtil;
import com.heatonresearch.aifh.general.fns.FnRBF;
import com.heatonresearch.aifh.general.fns.GaussianFunction;
import com.heatonresearch.aifh.randomize.GenerateRandom;

import java.util.Arrays;

/**
 * A RBF network is an advanced machine learning algorithm that uses a series of RBF functions to perform
 * regression.  It can also perform classification by means of one-of-n encoding.
 * <p/>
 * The long term memory of a RBF network is made up of the widths and centers of the RBF functions, as well as
 * input and output weighting.
 * <p/>
 * http://en.wikipedia.org/wiki/RBF_network
 */
public class RBFNetwork implements RegressionAlgorithm, ClassificationAlgorithm {

    /**
     * The input count.
     */
    private final int inputCount;

    /**
     * The output count.
     */
    private final int outputCount;

    /**
     * The RBF functions.
     */
    private final FnRBF[] rbf;
    private final double[] longTermMemory;
    private final int indexInputWeights;
    private final int indexOutputWeights;

    public RBFNetwork(int theInputCount, int rbfCount, int theOutputCount) {

        this.inputCount = theInputCount;
        this.outputCount = theOutputCount;

        // calculate input and output weight counts
        // add 1 to output to account for an extra bias node
        int inputWeightCount = inputCount * rbfCount;
        int outputWeightCount = (rbfCount + 1) * outputCount;
        int rbfParams = (inputCount + 1) * rbfCount;
        this.longTermMemory = new double[
                inputWeightCount + outputWeightCount + rbfParams];

        this.indexInputWeights = 0;
        this.indexOutputWeights = inputWeightCount + rbfParams;

        this.rbf = new FnRBF[rbfCount];

        for (int i = 0; i < rbfCount; i++) {
            int rbfIndex = inputWeightCount + ((inputCount + 1) * i);
            this.rbf[i] = new GaussianFunction(inputCount, this.longTermMemory, rbfIndex);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double[] computeRegression(double[] input) {

        // first, compute the output values of each of the RBFs
        // Add in one additional RBF output for bias (always set to one).
        double[] rbfOutput = new double[rbf.length + 1];
        rbfOutput[rbfOutput.length - 1] = 1; // bias

        for (int rbfIndex = 0; rbfIndex < rbf.length; rbfIndex++) {

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
                // add 1 to rbf length for bias
                int memoryIndex = this.indexOutputWeights + (outputIndex * (rbf.length + 1)) + rbfIndex;
                sum += rbfOutput[rbfIndex] * this.longTermMemory[memoryIndex];
            }
            result[outputIndex] = sum;
        }

        // finally, return the result.
        return result;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double[] getLongTermMemory() {
        return longTermMemory;
    }

    /**
     * Randomize the long term memory, with the specified random number generator.
     *
     * @param rnd A random number generator.
     */
    public void reset(GenerateRandom rnd) {
        for (int i = 0; i < this.longTermMemory.length; i++) {
            this.longTermMemory[i] = rnd.nextDouble(-1, 1);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int computeClassification(final double[] input) {
        double[] output = computeRegression(input);
        return VectorUtil.maxIndex(output);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        StringBuilder result = new StringBuilder();
        result.append("[RBFNetwork:inputCount=");
        result.append(this.inputCount);
        result.append(",outputCount=");
        result.append(this.outputCount);
        result.append(",RBFs=");
        result.append(Arrays.toString(this.rbf));
        result.append("]");
        return result.toString();
    }
}
