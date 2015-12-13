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
import com.heatonresearch.aifh.ann.Layer;
import com.heatonresearch.aifh.ann.train.error.CrossEntropyErrorFunction;
import com.heatonresearch.aifh.error.ErrorCalculation;
import com.heatonresearch.aifh.error.ErrorCalculationMSE;
import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.learning.LearningMethod;
import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

import java.util.List;

/**
 * This class supports several variants of the backpropagation training algorithm for neural networks.  By default,
 * this class will perform a stochastic gradient descent (SGD) train with a mini-batch of 500.  The cross-entropy
 * error function is used, along with Nesterov momentum. L1 & L2 regularization can also be used.
 *
 * With backpropagation is it important to choose a good learning rate and momentum.  If the learning rate is too high
 * your network will not converge, and may become unstable with weights going to NaN.  Too small a learning rate will
 * take a considerable amount of time to train.
 *
 * Nesterov, Y. (2004). Introductory lectures on convex optimization (Vol. 87). Springer Science & Business Media.
 *
 * Sutskever, Ilya, et al. "On the importance of initialization and momentum in deep learning." Proceedings of the
 * 30th international conference on machine learning (ICML-13). 2013.
 */
public class BackPropagation implements GradientCalcOwner, LearningMethod {

    /**
     * The network to train.
     */
    private final BasicNetwork network;

    /**
     * The training set.
     */
    private final List<BasicData> training;

    /**
     * The learning rate.
     */
    private final double learningRate;

    /**
     * The momentum.
     */
    private final double momentum;

    /**
     * The batch size, set to zero for full batch training.
     */
    private int batchSize = 500;

    /**
     * If we are doing non-stochastic batches, this keeps track of where we were in the
     * training set elements.
     */
    private int currentIndex;

    /**
     * Should we use stochastic gradient descent (SGD)?  If so, this holds the random number
     * generator.  If we do not desire SGD, set this value to null.
     */
    private GenerateRandom stochastic = new MersenneTwisterGenerateRandom();

    /**
     * Gradient calculation utility.
     */
    private final GradientCalc gradients;

    /**
     * The weight deltas from the last iteration.
     */
    private final double[] lastDelta;

    /**
     * The last error calculation.
     */
    private final ErrorCalculation errorCalc = new ErrorCalculationMSE();

    /**
     * The current error.
     */
    private double currentError = 1.0;

    /**
     * L1 regularization weighting, 0.0 for none.
     */
    private double l1;

    /**
     * L2 regularization weighting, 0.0 for none.
     */
    private double l2;

    /**
     * Should nesterov update be used?
     */
    private boolean nesterovUpdate = true;

    /**
     * Construct the backpropagation trainer.
     * @param theNetwork The network to train.
     * @param theTraining The training data to use.
     * @param theLearningRate The learning rate.  Can be changed as training runs.
     * @param theMomentum The momentum.  Can be changed as training runs.
     */
    public BackPropagation(BasicNetwork theNetwork, List<BasicData> theTraining, double theLearningRate,
                           double theMomentum) {
        this.network = theNetwork;
        this.training = theTraining;
        this.learningRate = theLearningRate;
        this.momentum = theMomentum;
        this.gradients = new GradientCalc(this.network, new CrossEntropyErrorFunction(), this);
        this.lastDelta = new double[theNetwork.getWeights().length];
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void iteration() {
        this.network.setNetworkTraining(true);

        // alert the layers that a new batch is starting.
        for(Layer layer: this.network.getLayers()) {
            layer.trainingBatch(this.stochastic);
        }

        // begin the iteration
        this.gradients.reset();
        this.errorCalc.clear();

        int iterationSize = this.batchSize==0 ? this.training.size()
                : Math.min(this.batchSize,this.training.size());


        for(int i=0;i<iterationSize;i++) {
            BasicData element;

            if( isOnlineTraining() ) {
                if( this.stochastic!=null ) {
                    int stochasticIndex = this.stochastic.nextInt(0,this.training.size());
                    element = this.training.get(stochasticIndex);
                } else {
                    element = this.training.get(this.currentIndex++);
                }
            } else {
                element = this.training.get(i);
            }
            this.gradients.process(this.errorCalc, element.getInput(), element.getIdeal());
        }

        if(this.currentIndex>this.training.size() || this.batchSize == 0) {
            this.currentIndex = 0;
        }

        this.currentError = this.errorCalc.calculate();

        for(int i=0;i<this.network.getWeights().length;i++) {
            double delta;

            if(this.nesterovUpdate) {
                double prevNesterov = this.lastDelta[i];

                this.lastDelta[i] = (this.momentum * prevNesterov)
                        + (this.gradients.getGradients()[i] * this.learningRate);
                delta = (this.momentum * prevNesterov) - ((1+this.momentum)*this.lastDelta[i]);
            } else {
                delta = (this.gradients.getGradients()[i] * -this.learningRate) + (this.lastDelta[i] * this.momentum);
                this.lastDelta[i] = delta;
            }

            this.network.getWeights()[i] += delta;
        }
        this.network.setNetworkTraining(false);
    }

    public boolean isOnlineTraining() {
        return this.batchSize!=0 && (this.batchSize<this.training.size());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double getLastError() {
        return this.currentError;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean done() {
        return false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getStatus() {
        return "";
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void finishTraining() {

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double getL1() {
        return this.l1;
    }

    public boolean isNesterovUpdate() {
        return this.nesterovUpdate;
    }

    public void setNesterovUpdate(boolean nesterovUpdate) {
        this.nesterovUpdate = nesterovUpdate;
    }

    /**
     * {@inheritDoc}
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

    /**
     * @return The batch size.
     */
    public int getBatchSize() {
        return this.batchSize;
    }

    /**
     * @return The learning rate.
     */
    public double getLearningRate() {
        return this.learningRate;
    }

    /**
     * @return The momentum.
     */
    public double getMomentum() {
        return this.momentum;
    }

    /**
     * The random number generator used for stochastic gradient descent (SGD), or null if none.
     * @return A random number generator, or null if not using SGD.
     */
    public GenerateRandom getStochastic() {
        return this.stochastic;
    }

    /**
     * The random number generator to use for stochastic gradient descent (CGD), or null for none.
     * @param stochastic Random number generator, or null.
     */
    public void setStochastic(GenerateRandom stochastic) {
        this.stochastic = stochastic;
    }

    /**
     * Set the batch size.
     * @param batchSize The batch size.
     */
    public void setBatchSize(int batchSize) {
        this.batchSize = batchSize;
    }
}
