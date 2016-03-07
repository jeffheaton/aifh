/*
 * Artificial Intelligence for Humans
 * Volume 3: Deep Learning and Neural Networks
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh
 *
 * Copyright 2014-2015 by Jeff Heaton
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * For more information on Heaton Research copyrights, licenses
 * and trademarks visit:
 * http://www.heatonresearch.com/copyright
 */
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

    /**
     * The weights & RBF parameters.  See constructor for layout.
     */
    private final double[] longTermMemory;

    /**
     * An index to the input weights in the long term memory.
     */
    private final int indexInputWeights;

    /**
     * An index to the output weights in the long term memory.
     */
    private final int indexOutputWeights;

    /**
     * Construct the RBF network.
     *
     * @param theInputCount  The input count.
     * @param rbfCount       The number of RBF functions.
     * @param theOutputCount The output count.
     */
    public RBFNetwork(final int theInputCount, final int rbfCount, final int theOutputCount) {

        this.inputCount = theInputCount;
        this.outputCount = theOutputCount;

        // calculate input and output weight counts
        // add 1 to output to account for an extra bias node
        final int inputWeightCount = this.inputCount * rbfCount;
        final int outputWeightCount = (rbfCount + 1) * this.outputCount;
        final int rbfParams = (this.inputCount + 1) * rbfCount;
        this.longTermMemory = new double[
                inputWeightCount + outputWeightCount + rbfParams];

        this.indexInputWeights = 0;
        this.indexOutputWeights = inputWeightCount + rbfParams;

        this.rbf = new FnRBF[rbfCount];

        for (int i = 0; i < rbfCount; i++) {
            final int rbfIndex = inputWeightCount + ((this.inputCount + 1) * i);
            this.rbf[i] = new GaussianFunction(this.inputCount, this.longTermMemory, rbfIndex);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double[] computeRegression(final double[] input) {

        // first, compute the output values of each of the RBFs
        // Add in one additional RBF output for bias (always set to one).
        final double[] rbfOutput = new double[this.rbf.length + 1];
        rbfOutput[rbfOutput.length - 1] = 1; // bias

        for (int rbfIndex = 0; rbfIndex < this.rbf.length; rbfIndex++) {

            // weight the input
            final double[] weightedInput = new double[input.length];

            for (int inputIndex = 0; inputIndex < input.length; inputIndex++) {
                final int memoryIndex = this.indexInputWeights + (rbfIndex * this.inputCount) + inputIndex;
                weightedInput[inputIndex] = input[inputIndex] * this.longTermMemory[memoryIndex];
            }

            // calculate the rbf
            rbfOutput[rbfIndex] = this.rbf[rbfIndex].evaluate(weightedInput);
        }

        // second, calculate the output, which is the result of the weighted result of the RBF's.
        final double[] result = new double[this.outputCount];

        for (int outputIndex = 0; outputIndex < result.length; outputIndex++) {
            double sum = 0;
            for (int rbfIndex = 0; rbfIndex < rbfOutput.length; rbfIndex++) {
                // add 1 to rbf length for bias
                final int memoryIndex = this.indexOutputWeights + (outputIndex * (this.rbf.length + 1)) + rbfIndex;
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
        return this.longTermMemory;
    }

    /**
     * Randomize the long term memory, with the specified random number generator.
     *
     * @param rnd A random number generator.
     */
    public void reset(final GenerateRandom rnd) {
        for (int i = 0; i < this.longTermMemory.length; i++) {
            this.longTermMemory[i] = rnd.nextDouble(-1, 1);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int computeClassification(final double[] input) {
        final double[] output = computeRegression(input);
        if (output.length > 1) {
            return VectorUtil.maxIndex(output);
        } else {
            return (int) output[0];
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        String result = "[RBFNetwork:inputCount=" +
                this.inputCount +
                ",outputCount=" +
                this.outputCount +
                ",RBFs=" +
                Arrays.toString(this.rbf) +
                "]";
        return result;
    }
}
