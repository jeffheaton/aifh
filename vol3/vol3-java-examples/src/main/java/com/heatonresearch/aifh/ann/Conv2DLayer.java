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
import com.heatonresearch.aifh.ann.activation.ActivationFunction;
import com.heatonresearch.aifh.ann.train.GradientCalc;
import com.heatonresearch.aifh.flat.FlatMatrix;
import com.heatonresearch.aifh.flat.FlatVolume;
import com.heatonresearch.aifh.randomize.GenerateRandom;

/**
 * A 2D convolution layer.
 *
 * LeCun, Y., Bottou, L., Bengio, Y., & Haffner, P. (1998). Gradient-based learning applied to document recognition.
 * Proceedings of the IEEE, 86(11), 2278-2324.
 */
public class Conv2DLayer implements Layer {

    /**
     * The number of filters (output depth).
     */
    private final int numFilters;

    /**
     * The filter size.
     */
    private final int filterSize;

    /**
     * The padding, both horizontal and vertical.
     */
    private final int padding;

    /**
     * The stride.
     */
    private final int stride;

    /**
     * The output columns.
     */
    private int outColumns;

    /**
     * The output rows.
     */
    private int outRows;

    /**
     * The input depth.
     */
    private int inDepth;

    private FlatVolume layerOutput;
    private FlatVolume layerSums;

    private final ActivationFunction activation;
    private int layerIndex;
    private BasicNetwork owner;
    private FlatMatrix[] weightMatrix;

    /**
     * Construct a 2D convolution layer.
     * @param theActivation The activation function to use.
     * @param theNumFilters The number of filters.
     * @param theFilterSize The size (rows & columns) of each filter, they must be square.
     * @param theStride The stride size.
     * @param thePadding The padding.
     */
    public Conv2DLayer(final ActivationFunction theActivation, int theNumFilters, int theFilterSize, int theStride,int thePadding) {
        this.activation = theActivation;
        this.filterSize = theFilterSize;
        this.numFilters = theNumFilters;
        this.stride = theStride;
        this.padding = thePadding;

    }

    /**
     * Construct a 2D convolution layer, with stride of 1 and padding of 0.
     * @param theActivation The activation function to use.
     * @param theNumFilters The number of filters.
     * @param theFilterSize The size (rows & columns) of each filter, they must be square.
     */
    public Conv2DLayer(final ActivationFunction theActivation, int theNumFilters, int theFilterSize) {
        this(theActivation, theNumFilters, theFilterSize,1,0);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void finalizeStructure(BasicNetwork theOwner, int theLayerIndex) {
        this.layerIndex = theLayerIndex;
        this.owner = theOwner;

        Layer prevLayer = (getLayerIndex()>0) ? getOwner().getLayers().get(getLayerIndex()-1) : null;
        Layer nextLayer = (getLayerIndex()<getOwner().getLayers().size()-1) ? getOwner().getLayers().get(getLayerIndex()+1) : null;

        if(prevLayer==null) {
            throw new AIFHError("Conv2DLayer must have a previous layer (cannot be used as the input layer).");
        }

        this.outRows = (int)Math.floor((prevLayer.getDimensionCounts()[0] + this.padding * 2 - this.filterSize) / this.stride + 1);
        this.outColumns = (int)Math.floor((prevLayer.getDimensionCounts()[1] + this.padding * 2 - this.filterSize) / this.stride + 1);

        int[] shape = {this.outRows, this.outColumns, this.numFilters};

        this.layerOutput = new FlatVolume(shape, true);
        this.layerSums = new FlatVolume(shape, true);
    }

    @Override
    public FlatVolume getLayerOutput() {
        return this.layerOutput;
    }

    @Override
    public FlatVolume getLayerSums() {
        return this.layerSums;
    }

    /**
     * @return The weight matrix.
     */
    @Override
    public FlatMatrix[] getWeightMatrix() {
        return this.weightMatrix;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getCount() {
        return this.outRows * this.outColumns;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getTotalCount() {
        return getCount()+1;
    }

    /**
     * @return The activation/transfer function for this neuron.
     */
    @Override
    public ActivationFunction getActivation() {
        return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void computeLayer() {
        Layer prev = getOwner().getPreviousLayer(this);

        for(int currentFilter=0;currentFilter<this.numFilters;currentFilter++) {

        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void computeGradient(GradientCalc calc) {

        // Calculate the output for each filter (depth).
        for(int dOutput=0;dOutput<this.numFilters;dOutput++) {
            for (int dInput = 0; dInput < this.inDepth; dInput++) {
                //computeGradient(calc);
            }
        }
    }

    /**
     * @return This layer's index in the layer stack.
     */
    @Override
    public int getLayerIndex() {
        return this.layerIndex;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void trainingBatch(GenerateRandom rnd) {
        // nothing to do
    }

    /**
     * @return The owner of the neural network.
     */
    @Override
    public BasicNetwork getOwner() {
        return this.owner;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isActive(int i) {
        return true;
    }

    /**
     * @return Conv2D layers always have bias.
     */
    @Override
    public boolean hasBias() {
        return true;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int[] getDimensionCounts() {
        return new int[] { this.filterSize, this.filterSize, this.numFilters };
    }

    /**
     * @return The padding.
     */
    public int getPadding() {
        return this.padding;
    }

    /**
     * @return The stride.
     */
    public int getStride() {
        return this.stride;
    }


}
