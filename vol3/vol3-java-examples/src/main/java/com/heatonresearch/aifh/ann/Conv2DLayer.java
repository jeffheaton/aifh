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
import com.heatonresearch.aifh.flat.FlatVolume;
import com.heatonresearch.aifh.randomize.GenerateRandom;

/**
 * A 2D convolution layer.
 *
 * LeCun, Y., Bottou, L., Bengio, Y., & Haffner, P. (1998). Gradient-based learning applied to document recognition.
 * Proceedings of the IEEE, 86(11), 2278-2324.
 */
public class Conv2DLayer extends WeightedLayer {

    /**
     * The number of filters (output depth).
     */
    private final int numFilters;

    /**
     * The number of rows in each filter.
     */
    private final int filterRows;

    /**
     * The number of columns in each filter.
     */
    private final int filterColumns;

    /**
     * The padding, both horizontal and vertical.
     */
    private int padding;

    /**
     * The stride.
     */
    private int stride = 1;

    /**
     * The output columns.
     */
    private double outColumns;

    /**
     * The output rows.
     */
    private double outRows;

    /**
     * The input depth.
     */
    private int inDepth;

    private FlatVolume layerOutput;
    private FlatVolume layerSums;


    /**
     * Construct a 2D convolution layer.
     * @param theActivation The activation function.
     * @param theNumFilters The number of filters.
     * @param theFilterRows The rows in each filter.
     * @param theFilterColumns The columns in each filter.
     */
    public Conv2DLayer(final ActivationFunction theActivation, int theNumFilters, int theFilterRows, int theFilterColumns) {
        this.setActivation(theActivation);
        this.filterRows = theFilterRows;
        this.filterColumns = theFilterColumns;
        this.numFilters = theNumFilters;
        int[] shape = {theFilterRows, theFilterColumns, theNumFilters};

        this.layerOutput = new FlatVolume(shape, true);
        this.layerSums = new FlatVolume(shape, true);

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void finalizeStructure(BasicNetwork theOwner, int theLayerIndex, TempStructureCounts counts) {
        super.finalizeStructure(theOwner,theLayerIndex,counts);

        Layer prevLayer = (getLayerIndex()>0) ? getOwner().getLayers().get(getLayerIndex()-1) : null;
        Layer nextLayer = (getLayerIndex()<getOwner().getLayers().size()-1) ? getOwner().getLayers().get(getLayerIndex()+1) : null;

        if(prevLayer==null) {
            throw new AIFHError("Conv2DLayer must have a previous layer (cannot be used as the input layer).");
        }

        int inColumns = prevLayer.getDimensionCounts()[0];
        int inRows = prevLayer.getDimensionCounts()[1];
        this.inDepth = prevLayer.getDimensionCounts()[2];

        this.outColumns = Math.floor((inColumns + this.padding * 2 - this.filterRows) / this.stride + 1);
        this.outRows = Math.floor((inRows + this.padding * 2 - this.filterColumns) / this.stride + 1);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getWeightDepthUnit() {
        Layer previousLayer = getOwner().getPreviousLayer(this);
        return previousLayer.getNeuronDepthUnit() * getNeuronDepthUnit();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getNeuronDepthUnit() {
        return this.filterColumns * this.filterRows;
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
     * {@inheritDoc}
     */
    @Override
    public int getCount() {
        return this.filterRows * this.filterColumns * this.numFilters;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getTotalCount() {
        return getCount()+1;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void computeLayer() {
        Layer prev = getOwner().getPreviousLayer(this);
        int fromCount;

        if( prev instanceof Conv2DLayer ) {
            fromCount = 1+((Conv2DLayer)prev).getFilterRows()*((Conv2DLayer)prev).getFilterRows();
        } else if( prev.getDimensionCounts().length==3) {
            fromCount = prev.getDimensionCounts()[0] * prev.getDimensionCounts()[1] + 1;
        } else {
            fromCount = prev.getCount();
        }

        // Calculate the output for each filter (depth).
        for(int dOutput=0;dOutput<this.numFilters;dOutput++) {
            for (int dInput = 0; dInput < this.inDepth; dInput++) {
                //computeLayer(dInput, dOutput, fromCount, getFilterColumns()*getFilterRows());
            }
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
                computeGradient(calc);
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void trainingBatch(GenerateRandom rnd) {
        // nothing to do
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
        return new int[] { this.filterColumns, this.filterRows, this.numFilters };
    }

    /**
     * @return The padding.
     */
    public int getPadding() {
        return this.padding;
    }

    /**
     * Set the padding.
     * @param padding The padding.
     */
    public void setPadding(int padding) {
        this.padding = padding;
    }

    /**
     * @return The stride.
     */
    public int getStride() {
        return this.stride;
    }

    /**
     * Set the stride.
     * @param stride The stride.
     */
    public void setStride(int stride) {
        this.stride = stride;
    }

    /**
     * @return The filter rows.
     */
    public int getFilterRows() { return this.filterRows; }

    /**
     * @return The filter columns.
     */
    public int getFilterColumns() { return this.filterColumns; }
}
