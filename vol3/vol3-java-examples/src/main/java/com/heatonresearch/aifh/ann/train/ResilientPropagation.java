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

import com.heatonresearch.aifh.ann.BasicNetwork;
import com.heatonresearch.aifh.ann.train.error.CrossEntropyErrorFunction;
import com.heatonresearch.aifh.error.ErrorCalculation;
import com.heatonresearch.aifh.error.ErrorCalculationMSE;
import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.learning.LearningMethod;

import java.util.List;

/**
 * One problem with the backpropagation algorithm is that the magnitude of the
 * partial derivative is usually too large or too small. Further, the learning
 * rate is a single value for the entire neural network. The resilient
 * propagation learning algorithm uses a special update value(similar to the
 * learning rate) for every neuron connection. Further these update values are
 * automatically determined, unlike the learning rate of the backpropagation
 * algorithm.
 *
 * There are a total of three parameters that must be provided to the resilient
 * training algorithm. Defaults are provided for each, and in nearly all cases,
 * these defaults are acceptable. This makes the resilient propagation algorithm
 * one of the easiest and most efficient training algorithms available.
 *
 * It is also important to note that RPROP does not work well with online training.
 * You should always use a batch size bigger than one.  Typically the larger the better.
 * By default a batch size of zero is used, zero means to include the entire training
 * set in the batch.
 *
 * The optional parameters are:
 *
 * zeroTolerance - How close to zero can a number be to be considered zero. The
 * default is 0.00000000000000001.
 *
 * initialUpdate - What are the initial update values for each matrix value. The
 * default is 0.1.
 *
 * maxStep - What is the largest amount that the update values can step. The
 * default is 50.xw
 *
 */
public class ResilientPropagation implements GradientCalcOwner, LearningMethod {

    /**
     * The network to train.
     */
    private final BasicNetwork network;

    /**
     * The training data.
     */
    private final List<BasicData> training;

    /**
     * The gradients.
     */
    private final GradientCalc gradients;

    /**
     * The weight delta from the last training iteration.
     */
    private final double[] lastDelta;

    /**
     * The gradients from the last training iteration.
     */
    private final double[] lastGradients;

    /**
     * The error calculation method to use.
     */
    private final ErrorCalculation errorCalc = new ErrorCalculationMSE();

    /**
     * The current error.
     */
    private double currentError = 1.0;

    /**
     * The L1 regularization.
     */
    private double l1;

    /**
     * The L2 regularization.
     */
    private double l2;

    /**
     * The current update values.
     */
    private final double[] updateValues;

    /**
     * The POSITIVE ETA value. This is specified by the resilient propagation
     * algorithm. This is the percentage by which the deltas are increased by if
     * the partial derivative is greater than zero.
     */
    public static final double POSITIVE_ETA = 1.2;

    /**
     * The NEGATIVE ETA value. This is specified by the resilient propagation
     * algorithm. This is the percentage by which the deltas are increased by if
     * the partial derivative is less than zero.
     */
    public static final double NEGATIVE_ETA = 0.5;

    /**
     * The minimum delta value for a weight matrix value.
     */
    public static final double DELTA_MIN = 1e-6;

    /**
     * The starting update for a delta.
     */
    public static final double DEFAULT_INITIAL_UPDATE = 0.1;

    /**
     * The maximum amount a delta can reach.
     */
    public static final double DEFAULT_MAX_STEP = 50;

    public ResilientPropagation(BasicNetwork theNetwork, List<BasicData> theTraining) {
        this.network = theNetwork;
        this.training = theTraining;
        this.gradients = new GradientCalc(this.network, new CrossEntropyErrorFunction(), this);
        this.lastDelta = new double[theNetwork.getWeights().length];
        this.updateValues = new double[theNetwork.getWeights().length];
        this.lastGradients = new double[theNetwork.getWeights().length];

        for (int i = 0; i < this.updateValues.length; i++) {
            this.updateValues[i] = ResilientPropagation.DEFAULT_INITIAL_UPDATE;
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void iteration() {
        this.gradients.reset();
        this.errorCalc.clear();

        // Calculate gradients for entire training set, RPROP does not do online.
        for (BasicData element : this.training) {
            this.gradients.process(this.errorCalc, element.getInput(), element.getIdeal());
        }
        this.currentError = this.errorCalc.calculate();

        // Apply the gradients according to the RPROP algorithm.
        for(int i=0;i<this.gradients.getGradients().length;i++) {
            double delta = calculateWeightDelta(this.gradients.getGradients(), this.lastGradients, i);
            this.lastGradients[i] = this.gradients.getGradients()[i];
            this.lastDelta[i] = delta;
            this.network.getWeights()[i]+=delta;
        }


    }

    /**
     * Calculate the change in weights.
     * @param gradients The gradients.
     * @param lastGradient The last graidents.
     * @param index The weight currently being updated.
     * @return The weight change.
     */
    public double calculateWeightDelta(final double[] gradients,
                                   final double[] lastGradient, final int index) {
        // multiply the current and previous gradient, and take the
        // sign. We want to see if the gradient has changed its sign.
        final int change = (int)Math.signum(gradients[index] * lastGradient[index]);
        double weightChange = 0;

        // if the gradient has retained its sign, then we increase the
        // delta so that it will converge faster
        if (change > 0) {
            double delta = this.updateValues[index]
                    * ResilientPropagation.POSITIVE_ETA;
            delta = Math.min(delta, ResilientPropagation.DEFAULT_MAX_STEP);
            weightChange = -Math.signum(gradients[index]) * delta;
            this.updateValues[index] = delta;
            lastGradient[index] = gradients[index];
        } else if (change < 0) {
            // if change<0, then the sign has changed, and the last
            // delta was too big
            double delta = this.updateValues[index]
                    * ResilientPropagation.NEGATIVE_ETA;
            delta = Math.max(delta, ResilientPropagation.DELTA_MIN);
            this.updateValues[index] = delta;
            weightChange = -this.lastDelta[index];
            // set the previous gradent to zero so that there will be no
            // adjustment the next iteration
            lastGradient[index] = 0;
        } else if (change == 0) {
            // if change==0 then there is no change to the delta
            final double delta = this.updateValues[index];
            weightChange = -Math.signum(gradients[index]) * delta;
            lastGradient[index] = gradients[index];
        }

        // apply the weight change, if any
        return weightChange;
    }

    /**
     * @return The error from the last training iteration.
     */
    @Override
    public double getLastError() {
        return this.currentError;
    }

    /**
     * @return True, if we are done learning.  Not all learning algorithms know when they are done, in this case
     * false is always returned.
     */
    @Override
    public boolean done() {
        return false;
    }

    /**
     * @return A string that indicates the status of training.
     */
    @Override
    public String getStatus() {
        return "";
    }

    /**
     * Should be called after the last iteration to make sure training completes any final tasks.
     */
    @Override
    public void finishTraining() {

    }

    /**
     * @return How much to apply l1 regularization penalty, 0 (default) for none.
     */
    @Override
    public double getL1() {
        return this.l1;
    }

    /**
     * @return How much to apply l2 regularization penalty, 0 (default) for none.
     */
    @Override
    public double getL2() {
        return this.l2;
    }

    /**
     * Set the L1 regularization multiplier.
     * @param theL1 The L1 regularization multiplier.
     */
    public void setL1(double theL1) {
        this.l1 = theL1;
    }

    /**
     * Set the L2 regularization multiplier.
     * @param theL2 The L2 regularization multiplier.
     */
    public void setL2(double theL2) {
        this.l2 = theL2;
    }

}
