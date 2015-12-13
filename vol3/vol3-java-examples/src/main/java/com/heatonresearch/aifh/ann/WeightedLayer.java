package com.heatonresearch.aifh.ann;

import com.heatonresearch.aifh.ann.activation.ActivationFunction;
import com.heatonresearch.aifh.ann.train.GradientCalc;

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

        int tc = getTotalCount();
        counts.addNeuronCount(tc);

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
            layerIndex = nextLayer.getNeuronIndex() + nextLayer.getTotalCount();

            //layerIndex = nextLayer.getLayerIndexReverse()
            //        + nextLayer.getTotalCount();
        }

        this.neuronIndex = layerIndex;
        this.weightIndex = weightIndex;
    }

    public void computeLayer(int inputOffset, int outputOffset, int fromCount, int toCount) {
        Layer prev = getOwner().getPreviousLayer(this);
        final double[] weights = getOwner().getWeights();
        int weightSize = getWeightDepthUnit();
        int outputSize = getNeuronDepthUnit();

        int index = getWeightIndex() + (inputOffset*weightSize);

        // weight values
        for (int ix = 0; ix < toCount; ix++) {
            int x = getNeuronIndex()+ix + (outputOffset * outputSize);
            double sum = 0;

            for (int y = 0; y < fromCount; y++) {
                if(prev.isActive(ix) && isActive(y)) {
                    sum += weights[index] * getOwner().getLayerOutput()[prev.getNeuronIndex()+y];
                }
                index++;
            }
            getOwner().getLayerSums()[x] += sum;
            getOwner().getLayerOutput()[x] += sum;
        }

        getActivation().activationFunction(
                getOwner().getLayerOutput(), getNeuronIndex(), toCount);
    }

    public void computeGradient(GradientCalc calc, int inputOffset, int outputOffset, int fromLayerSize, int toLayerSize) {
        Layer prev = getOwner().getPreviousLayer(this);
        final int fromLayerIndex = prev.getNeuronIndex();
        final int toLayerIndex = getNeuronIndex();
        final int weightSize = getWeightDepthUnit();
        final int outputSize = getNeuronDepthUnit();


        final int index = getWeightIndex()+(weightSize*inputOffset); // this.weightIndex[currentLevel];
        final ActivationFunction activation = getActivation();

        // handle weights
        // array references are made method local to avoid one indirection
        final double[] layerDelta = calc.getLayerDelta();
        final double[] weights = this.getOwner().getWeights();
        final double[] layerOutput = getOwner().getLayerOutput();
        final double[] layerSums = getOwner().getLayerSums();
        int y = fromLayerIndex;
        for (int yi = 0; yi < fromLayerSize; yi++) {
            final double output = layerOutput[y];
            double sum = 0;

            int wi = index + yi;

            for (int xi = 0; xi < toLayerSize; xi++, wi += fromLayerSize) {
                int x = xi + toLayerIndex;

                if (prev.isActive(yi) && isActive(xi))
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
