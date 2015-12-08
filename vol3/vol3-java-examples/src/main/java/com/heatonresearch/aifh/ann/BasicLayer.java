package com.heatonresearch.aifh.ann;

import com.heatonresearch.aifh.AIFHError;
import com.heatonresearch.aifh.ann.activation.ActivationFunction;
import com.heatonresearch.aifh.ann.train.GradientCalc;
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
                                  TempStructureCounts counts) {
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
    public void computeGradient(GradientCalc calc) {
        int currentLevel = getLayerIndexReverse();
        Layer prev = this.owner.getPreviousLayer(this);
        final int fromLayerIndex = prev.getNeuronIndex();
        final int toLayerIndex = getNeuronIndex();
        final int fromLayerSize = prev.getTotalCount();
        final int toLayerSize = getCount();

        final int index = getWeightIndex(); // this.weightIndex[currentLevel];
        final ActivationFunction activation = getActivation();

        // handle weights
        // array references are made method local to avoid one indirection
        final double[] layerDelta = calc.getLayerDelta();
        final double[] weights = this.getOwner().getWeights();
        final double[] layerOutput = this.owner.getLayerOutput();
        final double[] layerSums = this.owner.getLayerSums();
        int y = fromLayerIndex;
        for (int yi = 0; yi < fromLayerSize; yi++) {
            final double output = layerOutput[y];
            double sum = 0;

            int wi = index + yi;

            for (int xi = 0; xi < toLayerSize; xi++, wi += fromLayerSize) {
                int x = xi + toLayerIndex;

                if( prev.isActive(yi) && isActive(xi) )
                    calc.getGradients()[wi] += -(output * layerDelta[x]);
                sum += weights[wi] * layerDelta[x];
            }
            layerDelta[y] = sum
                    * (activation.derivativeFunction(layerSums[y], layerOutput[y]));

            y++;
        }
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
