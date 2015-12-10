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
        Layer next = getOwner().getNextLayer(this);

        // Calculate the output for each filter (depth).
        for(int dOutput=0;dOutput<this.numFilters;dOutput++) {
            for (int dInput = 0; dInput < this.inDepth; dInput++) {
                computeLayer(dInput, dOutput);
            }
        }
    }

    @Override
    public void computeGradient(GradientCalc calc) {

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
