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

    public void computeLayer(int inputOffset, int outputOffset) {
        Layer prev = getOwner().getPreviousLayer(this);
        final double[] weights = getOwner().getWeights();
        int weightSize = getWeightDepthUnit();
        int outputSize = getNeuronDepthUnit();

        int index = getWeightIndex() + (inputOffset*weightSize);

        // weight values
        for (int ix = 0; ix < getCount(); ix++) {
            int x = getNeuronIndex()+ix + (outputOffset * outputSize);
            double sum = 0;

            for (int y = 0; y < prev.getTotalCount(); y++) {
                if(prev.isActive(ix) && isActive(y)) {
                    sum += weights[index] * getOwner().getLayerOutput()[prev.getNeuronIndex()+y];
                }
                index++;
            }
            getOwner().getLayerSums()[x] = sum;
            getOwner().getLayerOutput()[x] = sum;
        }

        getActivation().activationFunction(
                getOwner().getLayerOutput(), getNeuronIndex(), getCount());
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
