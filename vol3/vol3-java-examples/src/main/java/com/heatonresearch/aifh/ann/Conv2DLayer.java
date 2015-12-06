package com.heatonresearch.aifh.ann;

import com.heatonresearch.aifh.ann.activation.ActivationFunction;
import com.heatonresearch.aifh.randomize.GenerateRandom;

public class Conv2DLayer implements Layer {

    /**
     * The activation function.
     */
    private ActivationFunction activation;

    /**
     * The neuron count.
     */
    private int[] count;

    private boolean hasBias;


    private BasicNetwork owner;

    private int numFilters;
    private int filterRows;
    private int filterColumns;



    public Conv2DLayer(final ActivationFunction theActivation, boolean theHasBias, int theNumFilters, int theFilterRows, int theFilterColumns) {
        this.activation = theActivation;
        this.hasBias = theHasBias;
        this.filterRows = theFilterRows;
        this.filterColumns = theFilterColumns;
        this.numFilters = theNumFilters;
    }

    @Override
    public int getCount() {
        return this.count[0];
    }

    @Override
    public int getTotalCount() {
        return 0;
    }

    @Override
    public ActivationFunction getActivation() {
        return this.activation;
    }

    @Override
    public void finalizeStructure(BasicNetwork theOwner, int theLayerIndex, int theNeuronIndex, int theWeightIndex) {
        this.owner = theOwner;
    }

    @Override
    public void computeLayer() {

    }

    @Override
    public int getWeightIndex() {
        return 0;
    }

    @Override
    public int getNeuronIndex() {
        return 0;
    }

    @Override
    public int getLayerIndex() {
        return 0;
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
}
