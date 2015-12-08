package com.heatonresearch.aifh.ann;

import com.heatonresearch.aifh.ann.activation.ActivationFunction;
import com.heatonresearch.aifh.ann.train.GradientCalc;
import com.heatonresearch.aifh.randomize.GenerateRandom;

public class Conv2DLayer extends WeightedLayer {

    /**
     * The activation function.
     */
    private ActivationFunction activation;


    private BasicNetwork owner;

    private int numFilters;
    private int filterRows;
    private int filterColumns;
    private int padding = 0;
    private int stride = 1;
    private int layerIndex;
    private int weightIndex;
    private int neuronIndex;



    public Conv2DLayer(final ActivationFunction theActivation, int theNumFilters, int theFilterRows, int theFilterColumns) {
        this.activation = theActivation;
        this.filterRows = theFilterRows;
        this.filterColumns = theFilterColumns;
        this.numFilters = theNumFilters;
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
    public ActivationFunction getActivation() {
        return this.activation;
    }

    @Override
    public void computeLayer() {
        Layer next = getOwner().getNextLayer(this);

        for(int d=0;d<this.numFilters;d++) {
            final double[] weights = getOwner().getWeights();

            int index = next.getWeightIndex();

            // weight values
            for (int ix = 0; ix < next.getCount(); ix++) {
                int x = next.getNeuronIndex() + ix;
                double sum = 0;

                for (int y = 0; y < getTotalCount(); y++) {
                    if (next.isActive(ix) && isActive(y)) {
                        sum += weights[index] * getOwner().getLayerOutput()[getNeuronIndex() + y];
                    }
                    index++;
                }
                getOwner().getLayerSums()[x] = sum;
                getOwner().getLayerOutput()[x] = sum;
            }
        }

        next.getActivation().activationFunction(
                getOwner().getLayerOutput(), next.getNeuronIndex(), next.getCount());
    }

    @Override
    public void computeGradient(GradientCalc calc) {

    }

    @Override
    public int getWeightIndex() {
        return this.weightIndex;
    }

    @Override
    public int getNeuronIndex() {
        return this.neuronIndex;
    }

    @Override
    public int getLayerIndexReverse() {
        return this.owner.getLayers().size() - 1 - this.layerIndex;
    }

    @Override
    public void trainingBatch(GenerateRandom rnd) {
        // nothing to do
    }

    @Override
    public BasicNetwork getOwner() {
        return this.owner;
    }

    @Override
    public boolean isActive(int i) {
        return true;
    }

    public boolean hasBias() {
        return true;
    }

    @Override
    public int getLayerIndex() { return 0; }

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
}
