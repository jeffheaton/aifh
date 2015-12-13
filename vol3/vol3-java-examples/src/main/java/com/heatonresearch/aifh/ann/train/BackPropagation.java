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

public class BackPropagation implements GradientCalcOwner, LearningMethod {

    private final BasicNetwork network;
    private final List<BasicData> training;
    private final double learningRate;
    private final double momentum;
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

    private final GradientCalc gradients;
    private final double[] lastDelta;
    private final ErrorCalculation errorCalc = new ErrorCalculationMSE();
    private double currentError = 1.0;
    private double l1;
    private double l2;

    private boolean nesterovUpdate = true;

    public BackPropagation(BasicNetwork theNetwork, List<BasicData> theTraining, double theLearningRate, double theMomentum) {
        this.network = theNetwork;
        this.training = theTraining;
        this.learningRate = theLearningRate;
        this.momentum = theMomentum;
        this.gradients = new GradientCalc(this.network, new CrossEntropyErrorFunction(), this);
        this.lastDelta = new double[theNetwork.getWeights().length];
    }

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

    public boolean isNesterovUpdate() {
        return this.nesterovUpdate;
    }

    public void setNesterovUpdate(boolean nesterovUpdate) {
        this.nesterovUpdate = nesterovUpdate;
    }

    /**
     * @return How much to apply l2 regularization penalty, 0 (default) for none.
     */
    @Override
    public double getL2() {
        return this.l2;
    }

    public void setL1(double theL1) {
        this.l1 = theL1;
    }

    public void setL2(double theL2) {
        this.l2 = theL2;
    }

    public int getBatchSize() {
        return this.batchSize;
    }

    public double getLearningRate() {
        return this.learningRate;
    }

    public double getMomentum() {
        return this.momentum;
    }

    public GenerateRandom getStochastic() {
        return this.stochastic;
    }

    public void setStochastic(GenerateRandom stochastic) {
        this.stochastic = stochastic;
    }

    public void setBatchSize(int batchSize) {
        this.batchSize = batchSize;
    }
}
