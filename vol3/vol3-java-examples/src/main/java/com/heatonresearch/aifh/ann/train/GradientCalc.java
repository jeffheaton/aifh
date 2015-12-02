package com.heatonresearch.aifh.ann.train;

import com.heatonresearch.aifh.AIFH;
import com.heatonresearch.aifh.ann.BasicNetwork;
import com.heatonresearch.aifh.ann.activation.ActivationFunction;
import com.heatonresearch.aifh.ann.train.error.ErrorFunction;
import com.heatonresearch.aifh.error.ErrorCalculation;

public class GradientCalc {
    /**
     * The network to train.
     */
    private final BasicNetwork network;

    /**
     * The actual values from the neural network.
     */
    private final double[] actual;

    /**
     * The deltas for each layer.
     */
    private final double[] layerDelta;

    /**
     * The neuron counts, per layer.
     */
    private final int[] layerCounts;

    /**
     * The feed counts, per layer.
     */
    private final int[] layerFeedCounts;

    /**
     * The layer indexes.
     */
    private final int[] layerIndex;

    /**
     * The index to each layer's weights and thresholds.
     */
    private final int[] weightIndex;

    /**
     * The output from each layer.
     */
    private final double[] layerOutput;

    /**
     * The sums.
     */
    private final double[] layerSums;

    /**
     * The gradients.
     */
    private final double[] gradients;

    /**
     * The weights and thresholds.
     */
    private final double[] weights;

    private GradientCalcOwner owner;

    /**
     * The error function to use.
     */
    private final ErrorFunction errorFunction;

    public GradientCalc(final BasicNetwork theNetwork,
                          ErrorFunction ef, GradientCalcOwner theOwner) {
        this.network = theNetwork;
        this.errorFunction = ef;

        this.layerDelta = new double[network.getLayerOutput().length];
        this.gradients = new double[network.getWeights().length];
        this.actual = new double[network.getOutputCount()];

        this.weights = network.getWeights();
        this.layerIndex = network.getLayerIndex();
        this.layerCounts = network.getLayerCounts();
        this.weightIndex = network.getWeightIndex();
        this.layerOutput = network.getLayerOutput();
        this.layerSums = network.getLayerSums();
        this.layerFeedCounts = network.getLayerFeedCounts();
        this.owner = theOwner;
    }

    /**
     * @return The network being processed.
     */
    public BasicNetwork getNetwork() {
        return this.network;
    }

    /**
     * @return The weights for this network.
     */
    public double[] getWeights() {
        return this.weights;
    }

    /**
     * Process one training set element.
     *
     * @param input
     *            The network input.
     * @param ideal
     *            The ideal values.
     */
    public void process(ErrorCalculation errorCalc, double[] input, double[] ideal) {
        this.network.compute(input, this.actual);

        errorCalc.updateError(this.actual, ideal, 1.0);

        // Calculate error for the output layer.
        this.errorFunction.calculateError(
                this.network.getActivationFunctions()[0], this.layerSums,this.layerOutput,
                ideal, this.actual, this.layerDelta, 0, 1.0);

        // Apply regularization, if requested.
        if( this.owner.getL1()> AIFH.DEFAULT_PRECISION
                || this.owner.getL1()>AIFH.DEFAULT_PRECISION  ) {
            double[] lp = new double[2];
            calculateRegularizationPenalty(lp);
            for(int i=0;i<this.actual.length;i++) {
                double p = (lp[0]*this.owner.getL1()) + (lp[1]*this.owner.getL2());
                this.layerDelta[i]+=p;
            }
        }

        // Propagate backwards (chain rule from calculus).
        for (int i = 0; i < this.layerCounts.length-1; i++) {
            processLevel(i);
        }
    }

    /**
     * Process one level.
     *
     * @param currentLevel
     *            The level.
     */
    private void processLevel(final int currentLevel) {
        final int fromLayerIndex = this.layerIndex[currentLevel + 1];
        final int toLayerIndex = this.layerIndex[currentLevel];
        final int fromLayerSize = this.layerCounts[currentLevel + 1];
        final int toLayerSize = this.layerFeedCounts[currentLevel];

        final int index = this.weightIndex[currentLevel];
        final ActivationFunction activation = this.network
                .getActivationFunctions()[currentLevel];

        // handle weights
        // array references are made method local to avoid one indirection
        final double[] layerDelta = this.layerDelta;
        final double[] weights = this.weights;
        final double[] gradients = this.gradients;
        final double[] layerOutput = this.layerOutput;
        final double[] layerSums = this.layerSums;
        int yi = fromLayerIndex;
        for (int y = 0; y < fromLayerSize; y++) {
            final double output = layerOutput[yi];
            double sum = 0;

            int wi = index + y;
            final int loopEnd = toLayerIndex+toLayerSize;

                for (int xi = toLayerIndex; xi < loopEnd; xi++, wi += fromLayerSize) {
                    this.gradients[wi] += -(output * layerDelta[xi]);
                    sum += weights[wi] * layerDelta[xi];
                }
                layerDelta[yi] = sum
                        * (activation.derivativeFunction(layerSums[yi], layerOutput[yi]));

            yi++;
        }
    }

    public void reset() {
        for(int i=0;i<this.gradients.length;i++) {
            this.gradients[i] = 0;
        }
    }


    /**
     * @return the gradients
     */
    public double[] getGradients() {
        return gradients;
    }

    public void calculateRegularizationPenalty(double[] l) {
        for (int i = 0; i < network.getLayerCounts().length - 1; i++) {
            layerRegularizationPenalty(i, l);
        }
    }

    public void layerRegularizationPenalty(final int fromLayer, final double[] l) {
        final int fromCount = network.getLayerTotalNeuronCount(fromLayer);
        final int toCount = network.getLayerNeuronCount(fromLayer + 1);

        for (int fromNeuron = 0; fromNeuron < fromCount; fromNeuron++) {
            for (int toNeuron = 0; toNeuron < toCount; toNeuron++) {
                double w = this.network.getWeight(fromLayer, fromNeuron, toNeuron);
                l[0]+=Math.abs(w);
                l[1]+=w*w;
            }
        }
    }
}
