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
import com.heatonresearch.aifh.randomize.GenerateRandom;

public class Conv2DLayer extends WeightedLayer {

    private int numFilters;
    private int filterRows;
    private int filterColumns;
    private int padding = 0;
    private int stride = 1;
    private double outColumns;
    private double outRows;
    private int inDepth;


    public Conv2DLayer(final ActivationFunction theActivation, int theNumFilters, int theFilterRows, int theFilterColumns) {
        this.setActivation(theActivation);
        this.filterRows = theFilterRows;
        this.filterColumns = theFilterColumns;
        this.numFilters = theNumFilters;
    }

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

    @Override
    public int getWeightDepthUnit() {
        Layer previousLayer = getOwner().getPreviousLayer(this);
        return previousLayer.getNeuronDepthUnit() * getNeuronDepthUnit();
    }

    @Override
    public int getNeuronDepthUnit() {
        return this.filterColumns * this.filterRows;
    }

    @Override
    public int getCount() {
        return this.filterRows * this.filterColumns * this.numFilters;
    }

    @Override
    public int getTotalCount() {
        return getCount()+1;
    }

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
                computeLayer(dInput, dOutput, fromCount, getFilterColumns()*getFilterRows());
            }
        }
    }

    @Override
    public void computeGradient(GradientCalc calc) {
        final Layer prev = getOwner().getPreviousLayer(this);
        final int fromLayerSize = prev.getTotalCount();
        final int toLayerSize = getCount();

        // Calculate the output for each filter (depth).
        for(int dOutput=0;dOutput<this.numFilters;dOutput++) {
            for (int dInput = 0; dInput < this.inDepth; dInput++) {
                computeGradient(calc,0,0,fromLayerSize,toLayerSize);
            }
        }
    }

    @Override
    public void trainingBatch(GenerateRandom rnd) {
        // nothing to do
    }

    @Override
    public boolean isActive(int i) {
        return true;
    }

    public boolean hasBias() {
        return true;
    }

    @Override
    public int[] getDimensionCounts() {
        return new int[] { this.filterColumns, this.filterRows, this.numFilters };
    }

    public int getPadding() {
        return padding;
    }

    public void setPadding(int padding) {
        this.padding = padding;
    }

    public int getStride() {
        return stride;
    }

    public void setStride(int stride) {
        this.stride = stride;
    }

    public int getFilterRows() { return this.filterRows; }
    public int getFilterColumns() { return this.filterColumns; }
}
