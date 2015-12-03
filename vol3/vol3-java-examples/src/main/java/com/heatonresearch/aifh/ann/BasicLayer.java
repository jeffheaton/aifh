package com.heatonresearch.aifh.ann;

import com.heatonresearch.aifh.ann.activation.ActivationFunction;
import com.sun.xml.internal.bind.annotation.OverrideAnnotationOf;

public class BasicLayer implements Layer {
    /**
     * The activation function.
     */
    private ActivationFunction activation;

    /**
     * The neuron count.
     */
    private int count;

    private boolean hasBias;

    private int layerIndex;

    private BasicNetwork owner;

    private int weightIndex;
    private int neuronIndex;

    /**
     * Do not use this constructor.  This was added to support serialization.
     */
    public BasicLayer() {

    }

    public BasicLayer(final ActivationFunction theActivation, boolean theHasBias, int theCount) {
        this.activation = theActivation;
        this.hasBias = theHasBias;
        this.count = theCount;
    }

    @Override
    public void finalizeStructure(BasicNetwork theOwner, int theLayerIndex,
                                  int theNeuronIndex, int theWeightIndex) {
        this.owner = theOwner;
        this.layerIndex = theLayerIndex;
        this.neuronIndex = theNeuronIndex;
        this.weightIndex = theWeightIndex;

    }


    /**
     * @return the activation
     */
    public ActivationFunction getActivation() {
        return this.activation;
    }

    /**
     * @return the count
     */
    public int getCount() {
        return this.count;
    }

    /**
     * @return The total number of neurons on this layer, includes context, bias
     *         and regular.
     */
    public int getTotalCount() {
        return getCount() + (hasBias() ? 1 : 0);
    }

    /**
     * @return the bias
     */
    public boolean hasBias() {
        return this.hasBias;
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
    public String toString() {
        final StringBuilder result = new StringBuilder();
        result.append("[");
        result.append(this.getClass().getSimpleName());
        result.append(": count=");
        result.append(this.count);
        result.append(",bias=").append(hasBias);

        result.append("]");
        return result.toString();
    }

    public void computeLayer() {
        Layer next = this.owner.getNextLayer(this);
        final int inputIndex = this.neuronIndex;
        final int outputIndex = next.getNeuronIndex();
        final int inputSize = getTotalCount();
        final int outputSize = next.getCount();
        final double[] weights = this.owner.getWeights();

        int index = next.getWeightIndex();

        final int limitX = outputIndex + outputSize;
        final int limitY = inputIndex + inputSize;

        // weight values
        for (int x = outputIndex; x < limitX; x++) {
            double sum = 0;
            for (int y = inputIndex; y < limitY; y++) {
                sum += weights[index++] * this.owner.getLayerOutput()[y];
            }
            this.owner.getLayerSums()[x] = sum;
            this.owner.getLayerOutput()[x] = sum;
        }

        next.getActivation().activationFunction(
                this.owner.getLayerOutput(), outputIndex, outputSize);

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
    public int getLayerIndex() {
        return this.layerIndex;
    }


}
