/*
 * Artificial Intelligence for Humans
 * Volume 3: Deep Learning and Neural Networks
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh
 *
 * Copyright 2014-2015 by Jeff Heaton
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * For more information on Heaton Research copyrights, licenses
 * and trademarks visit:
 * http://www.heatonresearch.com/copyright
 */
package com.heatonresearch.aifh.ann.train;

import com.heatonresearch.aifh.AIFH;
import com.heatonresearch.aifh.ann.BasicNetwork;
import com.heatonresearch.aifh.ann.Layer;
import com.heatonresearch.aifh.ann.activation.ActivationFunction;
import com.heatonresearch.aifh.ann.train.error.ErrorFunction;
import com.heatonresearch.aifh.error.ErrorCalculation;

/**
 * A utility class used to help calculate the gradient of the error function for neural networks.
 */
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

    /**
     * The owner of the gradient calculation.
     */
    private final GradientCalcOwner owner;

    /**
     * The error function to use.
     */
    private final ErrorFunction errorFunction;

    /**
     * Construct the gradient calculation class.
     * @param theNetwork The network to use.
     * @param ef The error function to use.
     * @param theOwner The owner (usually a trainer).
     */
    public GradientCalc(final BasicNetwork theNetwork,
                        ErrorFunction ef, GradientCalcOwner theOwner) {
        this.network = theNetwork;
        this.errorFunction = ef;

        this.layerDelta = new double[this.network.getLayerOutput().length];
        this.gradients = new double[this.network.getWeights().length];
        this.actual = new double[this.network.getOutputCount()];

        this.weights = this.network.getWeights();
        this.layerOutput = this.network.getLayerOutput();
        this.layerSums = this.network.getLayerSums();
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
     * @param  errorCalc The error calculation.
     * @param input The network input.
     * @param ideal The ideal values.
     */
    public void process(ErrorCalculation errorCalc, double[] input, double[] ideal) {
        this.network.compute(input, this.actual);

        errorCalc.updateError(this.actual, ideal, 1.0);

        // Calculate error for the output layer.
        int outputLayerIndex = this.network.getLayers().size() - 1;
        ActivationFunction outputActivation = this.network.getLayers().get(outputLayerIndex).getActivation();
        this.errorFunction.calculateError(
                outputActivation, this.layerSums, this.layerOutput,
                ideal, this.actual, this.layerDelta, 0, 1.0);

        // Apply regularization, if requested.
        if (this.owner.getL1() > AIFH.DEFAULT_PRECISION
                || this.owner.getL1() > AIFH.DEFAULT_PRECISION) {
            double[] lp = new double[2];
            calculateRegularizationPenalty(lp);
            for (int i = 0; i < this.actual.length; i++) {
                double p = (lp[0] * this.owner.getL1()) + (lp[1] * this.owner.getL2());
                this.layerDelta[i] += p;
            }
        }

        // Propagate backwards (chain rule from calculus).
        for (int i = this.network.getLayers().size() - 1; i > 0; i--) {
            Layer layer = this.network.getLayers().get(i);
            layer.computeGradient(this);
        }
    }


    /**
     * Reset all gradients to zero.
     */
    public void reset() {
        for (int i = 0; i < this.gradients.length; i++) {
            this.gradients[i] = 0;
        }
    }


    /**
     * @return the gradients
     */
    public double[] getGradients() {
        return this.gradients;
    }

    /**
     * Apply a regularization penalty, such as that from L1/L2 regularization.
     * @param l The penalty.
     */
    public void calculateRegularizationPenalty(double[] l) {
        for (int i = 0; i < this.network.getLayers().size() - 1; i++) {
            layerRegularizationPenalty(i, l);
        }
    }

    /**
     * Apply a regularization penalty, such as that from L1/L2 regularization.
     * @param fromLayer The from layer.
     * @param l The penalty.
     */
    public void layerRegularizationPenalty(final int fromLayer, final double[] l) {
        final int fromCount = this.network.getLayerTotalNeuronCount(fromLayer);
        final int toCount = this.network.getLayers().get(fromLayer + 1).getCount();

        for (int fromNeuron = 0; fromNeuron < fromCount; fromNeuron++) {
            for (int toNeuron = 0; toNeuron < toCount; toNeuron++) {
                double w = this.network.getWeight(fromLayer, fromNeuron, toNeuron);
                l[0] += Math.abs(w);
                l[1] += w * w;
            }
        }
    }

    /**
     * @return The layer deltas used to calculate the gradient.
     */
    public double[] getLayerDelta() {
        return this.layerDelta;
    }
}
