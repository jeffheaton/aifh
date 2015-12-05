package com.heatonresearch.aifh.ann;

import com.heatonresearch.aifh.AIFHError;
import com.heatonresearch.aifh.ann.activation.ActivationFunction;
import com.heatonresearch.aifh.randomize.GenerateRandom;

public class BasicLayer implements Layer {
    /**
     * The activation function.
     */
    private ActivationFunction activation;

    /**
     * The neuron count.
     */
    private int[] count;

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

    public BasicLayer(final ActivationFunction theActivation, boolean theHasBias, int[] theCount) {
        if( theCount.length!=1 && theCount.length!=3 ) {
            throw new AIFHError("The number of dimensions must be 1 or 3.");
        }
        this.activation = theActivation;
        this.hasBias = theHasBias;
        this.count = theCount;
    }

    public BasicLayer(final ActivationFunction theActivation, boolean theHasBias, int theCount) {
        this(theActivation,theHasBias,new int[] {theCount});
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
        int product = 1;
        for(int i=0;i<this.count.length;i++) {
            product*=this.count[i];
        }
        return product;
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

    @Override
    public void computeLayer() {
        Layer next = this.owner.getNextLayer(this);
        final double[] weights = this.owner.getWeights();

        int index = next.getWeightIndex();

        // weight values
        for (int ix = 0; ix < next.getCount(); ix++) {
            int x = next.getNeuronIndex()+ix;
            double sum = 0;

            for (int y = 0; y < getTotalCount(); y++) {
                if(next.isActive(ix) && isActive(y)) {
                    sum += weights[index] * this.owner.getLayerOutput()[this.neuronIndex+y];
                }
                index++;
            }
            this.owner.getLayerSums()[x] = sum;
            this.owner.getLayerOutput()[x] = sum;
        }

        next.getActivation().activationFunction(
                this.owner.getLayerOutput(), next.getNeuronIndex(), next.getCount());

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

    @Override
    public void trainingBatch(GenerateRandom rnd) {
        // Nothing needs to be done!
    }

    @Override
    public BasicNetwork getOwner() {
        return this.owner;
    }

    @Override
    public boolean isActive(int i) {
        return true;
    }

}
