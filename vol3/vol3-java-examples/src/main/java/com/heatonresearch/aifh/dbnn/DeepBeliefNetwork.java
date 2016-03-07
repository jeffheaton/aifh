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
package com.heatonresearch.aifh.dbnn;

import com.heatonresearch.aifh.AIFHError;
import com.heatonresearch.aifh.learning.RegressionAlgorithm;
import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

/**
 * A deep belief neural network.
 *
 * References:
 * http://deeplearning.net/software/theano/
 * https://github.com/yusugomori/DeepLearning
 * http://en.wikipedia.org/wiki/Deep_learning
 */
public class DeepBeliefNetwork implements RegressionAlgorithm {

    /**
     * The hidden layers of the neural network.
     */
    private final HiddenLayer[] layers;

    /**
     * The restricted boltzmann machines for the neural network, one for leach layer.
     */
    private final RestrictedBoltzmannMachine[] rbm;

    /**
     * The output layer for the neural network.
     */
    private final DeepLayer outputLayer;

    /**
     * The random number generator to use.
     */
    private GenerateRandom random = new MersenneTwisterGenerateRandom();

    /**
     * Construct a deep belief neural network.
     * @param inputCount The input count.
     * @param hidden The counts for the hidden layers.
     * @param outputCount The output neuron count.
     */
    public DeepBeliefNetwork(int inputCount, int[] hidden, int outputCount) {
        int inputSize;

        this.layers = new HiddenLayer[hidden.length];
        this.rbm = new RestrictedBoltzmannMachine[hidden.length];

        for (int i = 0; i < this.rbm.length; i++) {
            if (i == 0) {
                inputSize = inputCount;
            } else {
                inputSize = hidden[i - 1];
            }

            this.layers[i] = new HiddenLayer(this,inputSize, hidden[i]);

            this.rbm[i] = new RestrictedBoltzmannMachine(this.layers[i]);
        }

        this.outputLayer = new DeepLayer(this,hidden[this.layers.length - 1], outputCount);
    }

    /**
     * Randomize the weights of the neural network.
     */
    public void reset() {
        for (int i = 0; i < this.rbm.length; i++) {

            HiddenLayer layer = this.layers[i];

            double a = 1.0 / layer.getInputCount();

            for(int j=0; j<layer.getOutputCount(); j++) {
                for(int k=0; k<layer.getInputCount(); k++) {
                    layer.getWeights()[j][k] = getRandom().nextDouble(-a, a);
                }
            }
        }
    }

    /**
     * The sigmoid/logistic function, used by the output layer.
     * @param x The input.
     * @return The output.
     */
    public static double sigmoid(double x) {
        return 1.0 / (1.0 + Math.exp(-x));
    }

    /**
     * @return The layers of the neural network.
     */
    public HiddenLayer[] getLayers() {
        return this.layers;
    }

    /**
     * @return The restricted Boltzmann machines.
     */
    RestrictedBoltzmannMachine[] getRBMLayers() {
        return this.rbm;
    }

    /**
     * @return The input count.
     */
    public int getInputCount() {
        return this.layers[0].getInputCount();
    }

    /**
     * @return The output (logistic) layer.
     */
    public DeepLayer getLogLayer() {
        return this.outputLayer;
    }

    /**
     * @return The random number generator.
     */
    public GenerateRandom getRandom() {
        return this.random;
    }

    /**
     * Set the random number generator.
     * @param random The random number generator.
     */
    public void setRandom(final GenerateRandom random) {
        this.random = random;
    }

    /**
     * @return The number of output neurons.
     */
    public int getOutputCount() {
        return this.outputLayer.getOutputCount();
    }

    /**
     * Classify the input data into the list of probabilities of each class.
     * @param input The input.
     * @return An array that contains the probabilities of each class.
     */
    @Override
    public double[] computeRegression(final double[] input) {

        double[] result = new double[getOutputCount()];
        double[] layerInput = new double[0];
        double[] prevLayerInput = new double[getInputCount()];

        System.arraycopy(input, 0, prevLayerInput, 0, getInputCount());

        double output;

        for (int i = 0; i < this.layers.length; i++) {
            layerInput = new double[this.layers[i].getOutputCount()];

            for (int k = 0; k < this.layers[i].getOutputCount(); k++) {
                output = 0.0;

                for (int j = 0; j < this.layers[i].getInputCount(); j++) {
                    output += this.layers[i].getWeights()[k][j] * prevLayerInput[j];
                }
                output += this.layers[i].getBias()[k];
                layerInput[k] = sigmoid(output);
            }

            if (i < this.layers.length - 1) {
                prevLayerInput = new double[this.layers[i].getOutputCount()];
                System.arraycopy(layerInput, 0, prevLayerInput, 0, this.layers[i].getOutputCount());
            }
        }

        for (int i = 0; i < this.outputLayer.getOutputCount(); i++) {
            result[i] = 0;
            for (int j = 0; j < this.outputLayer.getInputCount(); j++) {
                result[i] += this.outputLayer.getWeights()[i][j] * layerInput[j];
            }
            result[i] += this.outputLayer.getBias()[i];
        }

        this.outputLayer.softmax(result);
        return result;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double[] getLongTermMemory() {
        throw new AIFHError("Can't access DBM memory as array.");
    }
}