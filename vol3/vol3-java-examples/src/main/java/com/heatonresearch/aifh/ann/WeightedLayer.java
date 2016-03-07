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

import com.heatonresearch.aifh.ann.activation.ActivationFunction;
import com.heatonresearch.aifh.ann.train.GradientCalc;

/**
 * Base class for all layers (used with BasicNetwork) that have weights.
 */
public abstract class WeightedLayer implements Layer {

    /**
     * The layer index.
     */
    private int layerIndex;

    /**
     * The network that owns this layer.
     */
    private BasicNetwork owner;

    /**
     * The index to this layer's weights.
     */
    private int weightIndex;

    /**
     * The index to this layer's neurons.
     */
    private int neuronIndex;

    /**
     * The activation function.
     */
    private ActivationFunction activation;

    /**
     * {@inheritDoc}
     */
    @Override
    public void finalizeStructure(BasicNetwork theOwner, int theLayerIndex, TempStructureCounts counts) {
        this.owner = theOwner;
        this.layerIndex = theLayerIndex;

        Layer prevLayer = (this.layerIndex>0) ? this.owner.getLayers().get(this.layerIndex-1) : null;
        Layer nextLayer = (this.layerIndex<this.owner.getLayers().size()-1) ? this.owner.getLayers().get(this.layerIndex+1) : null;

        int tc = getTotalCount();
        counts.addNeuronCount(tc);

        if (prevLayer != null) {
            counts.addWeightCount(getCount() * prevLayer.getTotalCount());
        }

        int weightIndex, layerIndex;
        if (theLayerIndex == this.owner.getLayers().size()-1 ) {
            weightIndex = 0;
            layerIndex = 0;
        } else {
            weightIndex = nextLayer.getWeightIndex()
                    + (getTotalCount() * nextLayer.getCount());
            layerIndex = nextLayer.getNeuronIndex() + nextLayer.getTotalCount();

            //layerIndex = nextLayer.getLayerIndexReverse()
            //        + nextLayer.getTotalCount();
        }

        this.neuronIndex = layerIndex;
        this.weightIndex = weightIndex;
    }

    /**
     * Compute a layer.
     * @param inputOffset The offset to the input for this layer.
     * @param outputOffset The offset to the output from this layer.
     * @param fromCount The count of from neurons.
     * @param toCount The count of to neurons.
     */
    public void computeLayer(int inputOffset, int outputOffset, int fromCount, int toCount) {
        Layer prev = getOwner().getPreviousLayer(this);
        final double[] weights = getOwner().getWeights();

        int index = getWeightIndex();

        // weight values
        for (int ix = 0; ix < toCount; ix++) {
            int x = getNeuronIndex()+ix;
            double sum = 0;

            for (int y = 0; y < fromCount; y++) {
                sum += weights[index] * getOwner().getLayerOutput()[prev.getNeuronIndex()+y];
                index++;
            }
            getOwner().getLayerSums()[x] += sum;
            getOwner().getLayerOutput()[x] += sum;
        }

        getActivation().activationFunction(
                getOwner().getLayerOutput(), getNeuronIndex(), toCount);
    }

    /**
     * Compute gradients for this layer.
     * @param calc The gradient calculator.
     * @param inputOffset The input offset.
     * @param outputOffset The output offset.
     * @param fromLayerSize The from layer size.
     * @param toLayerSize The to layer size.
     */
    public void computeGradient(GradientCalc calc, int inputOffset, int outputOffset, int fromLayerSize, int toLayerSize) {
        Layer prev = getOwner().getPreviousLayer(this);
        final int fromLayerIndex = prev.getNeuronIndex();
        final int toLayerIndex = getNeuronIndex();


        final int index = getWeightIndex();
        final ActivationFunction activation = getActivation();

        // handle weights
        // array references are made method local to avoid one indirection
        final double[] layerDelta = calc.getLayerDelta();
        final double[] weights = this.getOwner().getWeights();
        final double[] layerOutput = getOwner().getLayerOutput();
        final double[] layerSums = getOwner().getLayerSums();
        int y = fromLayerIndex;
        for (int yi = 0; yi < fromLayerSize; yi++) {
            final double output = layerOutput[y];
            double sum = 0;

            int wi = index + yi;

            for (int xi = 0; xi < toLayerSize; xi++, wi += fromLayerSize) {
                int x = xi + toLayerIndex;

                calc.getGradients()[wi] += -(output * layerDelta[x]);
                sum += weights[wi] * layerDelta[x];
            }
            layerDelta[y] = sum
                    * (activation.derivativeFunction(layerSums[y], layerOutput[y]));

            y++;
        }
    }


    /**
     * {@inheritDoc}
     */
    @Override
    public int getWeightIndex() {
        return this.weightIndex;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getNeuronIndex() {
        return this.neuronIndex;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BasicNetwork getOwner() {
        return this.owner;
    }

    /**
     * @param activation
     *            the activation to set
     */
    public void setActivation(final ActivationFunction activation) {
        this.activation = activation;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ActivationFunction getActivation() {
        return this.activation;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        final StringBuilder result = new StringBuilder();
        result.append("[");
        result.append(this.getClass().getSimpleName());
        result.append(",count=").append(getCount());
        result.append(",weightIndex=").append(getWeightIndex());
        result.append(",neuronIndex=").append(getNeuronIndex());

        result.append("]");
        return result.toString();
    }
}
