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
package com.heatonresearch.aifh.ann;

import com.heatonresearch.aifh.AIFHError;
import com.heatonresearch.aifh.ann.randomize.XaiverRandomizeNetwork;
import com.heatonresearch.aifh.learning.ClassificationAlgorithm;
import com.heatonresearch.aifh.learning.RegressionAlgorithm;
import com.heatonresearch.aifh.util.ArrayUtil;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * The base class for most feedforward networks in this book.  This includes deep and convolutional networks.
 *
 * Layers must be added to the neural network and then a call to finalizeStructure makes the network ready for use.
 * After the call to finalizeStructure, layers can no longer be added to the NN.
 */
public class BasicNetwork implements RegressionAlgorithm, ClassificationAlgorithm {

    /**
     * The number of input neurons in this network.
     */
    private int inputCount;

    /**
     * The outputs from each of the neurons.
     */
    private double[] layerOutput;

    /**
     * The sum of the layer, before the activation function is applied, producing the layerOutput.
     */
    private double[] layerSums;

    /**
     * The number of output neurons in this network.
     */
    private int outputCount;

    /**
     * The weights for a neural network.
     */
    private double[] weights;

    /**
     * The layers of the network.
     */
    private final List<Layer> layers = new ArrayList<>();

    /**
     * True if the network is in training mode.  Some layers act differently while training (i.e. dropout).
     */
    private boolean networkTraining;

    /**
     * Default constructor.
     */
    public BasicNetwork() {
    }


    /**
     * Calculate the output for the given input.
     *
     * @param input
     *            The input.
     * @param output
     *            Output will be placed here.
     */
    public void compute(final double[] input, final double[] output) {
        clearOutput();

        final int sourceIndex = getNeuronCount()
                - this.layers.get(0).getTotalCount();

        System.arraycopy(input, 0, this.layerOutput, sourceIndex,
                this.inputCount);

        for (int i = 1; i<this.layers.size(); i++) {
            this.layers.get(i).computeLayer();
        }


        System.arraycopy(this.layerOutput, 0, output, 0, this.outputCount);
    }

    /**
     * @return The total number of neurons in the neural network.
     */
    public int getNeuronCount() {
        int result = 0;
        for(Layer layer: this.layers) {
            result+=layer.getTotalCount();
        }
        return result;
    }

    /**
     * @return The length of the array the network would encode to.
     */
    public int getEncodeLength() {
        return this.weights.length;
    }


    /**
     * @return The number of input neurons.
     */
    public int getInputCount() {
        return this.inputCount;
    }

    /**
     * @return The output for each layer.
     */
    public double[] getLayerOutput() {
        return this.layerOutput;
    }

    /**
     * @return The number of output neurons.
     */
    public int getOutputCount() {
        return this.outputCount;
    }

    /**
     * @return The index of each layer in the weight and threshold array.
     */
    public double[] getWeights() {
        return this.weights;
    }


    /**
     * Set the input count.
     * @param inputCount The input count.
     */
    public void setInputCount(final int inputCount) {
        this.inputCount = inputCount;
    }



    /**
     * Set the output count.
     * @param outputCount The output count.
     */
    public void setOutputCount(final int outputCount) {
        this.outputCount = outputCount;
    }


    /**
     * @return the layerSums
     */
    public double[] getLayerSums() {
        return this.layerSums;
    }

    /**
     * Get the weight between the two layers.
     * @param fromLayer The from layer.
     * @param fromNeuron The from neuron.
     * @param toNeuron The to neuron.
     * @return The weight value.
     */
    public double getWeight(final int fromLayer,
                            final int fromNeuron,
                            final int toNeuron) {
        validateNeuron(fromLayer, fromNeuron);
        validateNeuron(fromLayer + 1, toNeuron);
        final int fromLayerNumber = this.layers.size() - fromLayer - 1;
        final int toLayerNumber = fromLayerNumber - 1;

        if (toLayerNumber < 0) {
            throw new AIFHError(
                    "The specified layer is not connected to another layer: "
                            + fromLayer);
        }

        final int weightBaseIndex
                = this.layers.get(fromLayer+1).getWeightIndex();
        final int count = this.layers.get(fromLayer).getTotalCount();
        final int weightIndex = weightBaseIndex + fromNeuron
                + (toNeuron * count);

        return this.weights[weightIndex];
    }

    /**
     * Validate the the specified targetLayer and neuron are valid.
     * @param targetLayer The target layer.
     * @param neuron The target neuron.
     */
    public void validateNeuron(final int targetLayer, final int neuron) {
        if ((targetLayer < 0) || (targetLayer >= this.layers.size())) {
            throw new AIFHError("Invalid layer count: " + targetLayer);
        }

        if ((neuron < 0) || (neuron >= getLayerTotalNeuronCount(targetLayer))) {
            throw new AIFHError("Invalid neuron number: " + neuron);
        }
    }

    /**
     * Get the total (including bias and context) neuron cont for a layer.
     * @param l The layer.
     * @return The count.
     */
    public int getLayerTotalNeuronCount(final int l) {
        return this.layers.get(l).getTotalCount();
    }


    /**
     * Add a layer to the neural network.
     * @param layer The layer to be added to the neural network.
     */
    public void addLayer(Layer layer) {
        this.layers.add(layer);
    }

    /**
     * Finalize the structure of the neural network.  This must be called before any training or calculation can
     * be performed.  After this method is called layers can no longer be added to the neural network.
     */
    public void finalizeStructure() {
        final int layerCount = this.layers.size();

        this.inputCount = this.layers.get(0).getCount();
        this.outputCount = this.layers.get(layerCount - 1).getCount();

        TempStructureCounts counts = new TempStructureCounts();

        for (int i = this.layers.size() - 1; i >= 0; i--) {

            final Layer layer = this.layers.get(i);

            layer.finalizeStructure(this, i, counts);

        }

        this.weights = new double[counts.getWeightCount()];
        this.layerOutput = new double[counts.getNeuronCount()];
        this.layerSums = new double[counts.getNeuronCount()];

        clearOutput();
    }

    /**
     * Clear the outputs of each layer.
     */
    public void clearOutput() {
        // Clear all outputs to 0
        for(int i=0;i<this.layerOutput.length;i++) {
            this.layerOutput[i] = 0.0;
            this.layerSums[i] = 0.0;
        }
        // Init the output arrays by filling in bias values
        int index = 0;
        for (int i = 0; i < this.layers.size(); i++) {
            Layer layer = this.layers.get(this.layers.size()-1-i);
            index += layer.getCount();
            if (layer.hasBias()) {
                this.layerOutput[index++] = 1.0;
            }
        }
    }

    /**
     * Set the weight between the two specified neurons. The bias neuron is always
     * the last neuron on a layer.
     * @param fromLayer The from layer.
     * @param fromNeuron The from neuron.
     * @param toNeuron The to neuron.
     * @param value The to value.
     */
    public void setWeight(final int fromLayer, final int fromNeuron,
                          final int toNeuron, final double value) {
        final int fromLayerNumber = this.layers.size() - fromLayer - 1;
        final int toLayerNumber = fromLayerNumber - 1;

        if (toLayerNumber < 0) {
            throw new AIFHError(
                    "The specified layer is not connected to another layer: "
                            + fromLayer);
        }

        final int weightBaseIndex
                = this.layers.get(fromLayer+1).getWeightIndex();
        final int count = this.layers.get(fromLayer).getTotalCount();

        final int weightIndex = weightBaseIndex + fromNeuron
                + (toNeuron * count);

        getWeights()[weightIndex] = value;
    }

    /**
     * Randomize the neural network.
     */
    public void reset() {
        XaiverRandomizeNetwork random = new XaiverRandomizeNetwork();
        random.randomize(this);
    }

    /**
     * @return The layers of the neural network.
     */
    public List<Layer> getLayers() {
        return this.layers;
    }

    /**
     * Compute the output for the specified input.
     *
     * @param input The input.
     * @return The regression output.
     */
    @Override
    public double[] computeRegression(double[] input) {
        if( input.length!=getInputCount()) {
            throw new AIFHError("Invalid input count("+ input.length+"), this network is designed for: "
                    + getInputCount());
        }
        double[] output = new double[getOutputCount()];
        compute(input,output);
        return output;
    }

    /**
     * @return The long term memory for the algorithm.  This is usually weights or other coefficients.
     */
    @Override
    public double[] getLongTermMemory() {
        return this.weights;
    }


    /**
     * Find the next layer in a neural network, given a layer.
     * @param layer The reference layer.
     * @return The next layer in the neural network.
     */
    public Layer getNextLayer(Layer layer) {
        int idx = this.layers.indexOf(layer);
        if( idx==-1 ) {
            throw new AIFHError("Can't find next layer for a layer that is not part of this network.");
        }
        if( idx>=this.layers.size() ) {
            throw new AIFHError("Can't find the next layer for the final layer in a network.");
        }
        return this.layers.get(idx+1);
    }

    /**
     * Find the previous layer in a neural network, given a layer.
     * @param layer The reference layer.
     * @return The previous layer in the neural network.
     */
    public Layer getPreviousLayer(Layer layer) {
        int idx = this.layers.indexOf(layer);
        if( idx==-1 ) {
            throw new AIFHError("Can't find previous layer for a layer that is not part of this network.");
        }
        if( idx==0 ) {
            throw new AIFHError("Can't find the previous layer for the final layer in a network.");
        }
        return this.layers.get(idx-1);
    }

    /**
     * Classify the specified input into a group.
     *
     * @param input The input data.
     * @return The group the data was classified into.
     */
    @Override
    public int computeClassification(double[] input) {
        return ArrayUtil.indexOfLargest(computeRegression(input));
    }

    /**
     * @return True, if the neural network is training.  Some layers (e.g. dropout) behave differently.
     */
    public boolean isNetworkTraining() {
        return this.networkTraining;
    }

    /**
     * Determine if the neural network is training.
     * @param networkTraining True, if the neural network is training.
     */
    public void setNetworkTraining(boolean networkTraining) {
        this.networkTraining = networkTraining;
    }
}
