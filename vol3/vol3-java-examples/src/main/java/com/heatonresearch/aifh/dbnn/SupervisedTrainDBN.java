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
package com.heatonresearch.aifh.dbnn;

import com.heatonresearch.aifh.error.ErrorCalculation;
import com.heatonresearch.aifh.error.ErrorCalculationMSE;
import com.heatonresearch.aifh.learning.LearningMethod;

/**
 * Supervised training for the DBN.  Used to train the output layer with labels.
 */
public class SupervisedTrainDBN implements LearningMethod {

    /**
     * The network to train.
     */
    private final DeepBeliefNetwork network;

    /**
     * The input (x) for the training.
     */
    private final double[][] trainingInput;

    /**
     * The expected output (y, or labels).
     */
    private final double[][] trainingIdeal;

    /**
     * The learning rate.
     */
    private final double learningRate;

    /**
     * The error calculation to use.
     */
    private ErrorCalculation errorCalc = new ErrorCalculationMSE();

    /**
     * Construct the supervised trainer for DBN.
     * @param theNetwork The network to train.
     * @param theTrainingInput The input (x) to train.
     * @param theTrainingIdeal The expected output (y, or labels) to train.
     * @param theLearningRate The learning rate.
     */
    public SupervisedTrainDBN(DeepBeliefNetwork theNetwork, double[][] theTrainingInput, double[][] theTrainingIdeal,
                                double theLearningRate) {
        this.network = theNetwork;
        this.trainingInput = theTrainingInput;
        this.learningRate = theLearningRate;
        this.trainingIdeal = theTrainingIdeal;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void iteration() {
        double[] layerInput = new double[0];
        double[] prevLayerInput;

        this.errorCalc.clear();
            for (int n = 0; n < this.trainingInput.length; n++) {

                for (int i = 0; i < this.network.getLayers().length; i++) {
                    if (i == 0) {
                        prevLayerInput = new double[this.network.getInputCount()];
                        System.arraycopy(this.trainingInput[n], 0, prevLayerInput, 0, this.network.getInputCount());
                    } else {
                        prevLayerInput = new double[this.network.getLayers()[i].getInputCount()];
                        System.arraycopy(layerInput, 0, prevLayerInput, 0, this.network.getLayers()[i].getInputCount());
                    }

                    layerInput = new double[this.network.getLayers()[i].getOutputCount()];
                    this.network.getLayers()[i].sampleHgivenV(prevLayerInput, layerInput);
                }

                trainLogisticLayer(layerInput, this.trainingIdeal[n]);
            }
        }

    /**
     * {@inheritDoc}
     */
    @Override
    public double getLastError() {
        return this.errorCalc.calculate();
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
     * Train the logistic layer, the output layer.
     * @param input The input (x).
     * @param ideal The expected output (y, or labels).
     */
    private void trainLogisticLayer(double[] input, double[] ideal) {
        double[] pYgivenX = new double[this.network.getLogLayer().getOutputCount()];
        double[] dy = new double[this.network.getLogLayer().getOutputCount()];

        for(int i=0; i<this.network.getLogLayer().getOutputCount(); i++) {
            pYgivenX[i] = 0;
            for(int j=0; j<this.network.getLogLayer().getInputCount(); j++) {
                pYgivenX[i] += this.network.getLogLayer().getWeights()[i][j] * input[j];
            }
            pYgivenX[i] += this.network.getLogLayer().getBias()[i];
        }
        this.network.getLogLayer().softmax(pYgivenX);



        for(int i=0; i<this.network.getLogLayer().getOutputCount(); i++) {
            dy[i] = ideal[i] - pYgivenX[i];
            this.errorCalc.updateError(ideal[i], pYgivenX[i]);

            for(int j=0; j<this.network.getLogLayer().getInputCount(); j++) {
                this.network.getLogLayer().getWeights()[i][j] += this.learningRate * dy[i] * input[j] / this.trainingInput.length;
            }

            this.network.getLogLayer().getBias()[i] += this.learningRate * dy[i] / this.trainingInput.length;
        }
    }

    /**
     * @return The error calculation method.
     */
    public ErrorCalculation getErrorCalc() {
        return this.errorCalc;
    }

    /**
     * Set the error calculation method.
     * @param errorCalc The error calculation method.
     */
    public void setErrorCalc(final ErrorCalculation errorCalc) {
        this.errorCalc = errorCalc;
    }
}
