package com.heatonresearch.aifh.ann;

import com.heatonresearch.aifh.ann.activation.ActivationFunction;

public abstract class WeightedLayer implements Layer {
    private int layerIndex;
    private BasicNetwork owner;
    private int weightIndex;
    private int neuronIndex;
    /**
     * The activation function.
     */
    private ActivationFunction activation;

    @Override
    public void finalizeStructure(BasicNetwork theOwner, int theLayerIndex, TempStructureCounts counts) {
        this.owner = theOwner;
        this.layerIndex = theLayerIndex;

        Layer prevLayer = (this.layerIndex>0) ? this.owner.getLayers().get(this.layerIndex-1) : null;
        Layer nextLayer = (this.layerIndex<this.owner.getLayers().size()-1) ? this.owner.getLayers().get(this.layerIndex+1) : null;

        counts.addNeuronCount(getTotalCount());

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
            layerIndex = nextLayer.getLayerIndexReverse()
                    + nextLayer.getTotalCount();
        }

        this.neuronIndex = layerIndex;
        this.weightIndex = weightIndex;
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

    public int getLayerIndex() { return this.layerIndex; }

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
     * @return the activation
     */
    public ActivationFunction getActivation() {
        return this.activation;
    }
}
