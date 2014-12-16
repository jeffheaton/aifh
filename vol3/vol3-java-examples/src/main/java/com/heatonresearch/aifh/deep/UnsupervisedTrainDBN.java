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
package com.heatonresearch.aifh.deep;

public class UnsupervisedTrainDBN {

    private DeepBeliefNetwork network;
    private int level;
    private double[][] trainingInput;
    private double learningRate;
    private int k;

    public UnsupervisedTrainDBN(DeepBeliefNetwork theNetwork, int theLevel, double[][] theTrainingInput,
                                double theLearningRate, int theK) {
        this.network = theNetwork;
        this.level = theLevel;
        this.trainingInput = theTrainingInput;
        this.learningRate = theLearningRate;
        this.k = theK;
    }

    public void iteration() {
        double[] layerInput = new double[0];
        int prevLayerInputSize;
        double[] prevLayerInput;


        for (final double[] aTrainingInput : trainingInput) {
            for (int l = 0; l <= level; l++) {

                if (l == 0) {
                    layerInput = new double[this.network.getInputCount()];
                    System.arraycopy(aTrainingInput, 0, layerInput, 0, this.network.getInputCount());
                } else {
                    if (l == 1) prevLayerInputSize = this.network.getInputCount();
                    else prevLayerInputSize = this.network.getLayers()[l - 1].getInputCount();

                    prevLayerInput = new double[prevLayerInputSize];
                    System.arraycopy(layerInput, 0, prevLayerInput, 0, prevLayerInputSize);

                    layerInput = new double[this.network.getLayers()[l].getInputCount()];

                    this.network.getLayers()[l - 1].sampleHgivenV(prevLayerInput, layerInput);
                }
            }

            contrastiveDivergence(this.network.getRBMLayers()[this.level], layerInput, this.learningRate, k, this.trainingInput.length);
        }
    }

    public void contrastiveDivergence(RestrictedBoltzmannMachine rbm, double[] input, double lr, int k, int n) {
        double[] meanPH = new double[rbm.getHiddenCount()];
        double[] samplePH = new double[rbm.getHiddenCount()];
        double[] meansNV = new double[rbm.getVisibleCount()];
        double[] samplesNV = new double[rbm.getVisibleCount()];
        double[] meansNH = new double[rbm.getHiddenCount()];
        double[] samplesNH = new double[rbm.getHiddenCount()];

        sampleHV(rbm, input, meanPH, samplePH);

        for(int step=0; step<k; step++) {
            if(step == 0) {
                gibbsHVH(rbm, samplePH, meansNV, samplesNV, meansNH, samplesNH);
            } else {
                gibbsHVH(rbm, samplesNH, meansNV, samplesNV, meansNH, samplesNH);
            }
        }

        for(int i=0; i<rbm.getHiddenCount(); i++) {
            for(int j=0; j<rbm.getVisibleCount(); j++) {
                rbm.getLayer().getWeights()[i][j] += lr *(meanPH[i] * input[j] - meansNH[i] * samplesNV[j]) / n;
            }
            rbm.getBiasH()[i] += lr * (samplePH[i] - meansNH[i]) / n;
        }


        for(int i=0; i<rbm.getVisibleCount(); i++) {
            rbm.getBiasV()[i] += lr * (input[i] - samplesNV[i]) / n;
        }
    }

    public void sampleHV(RestrictedBoltzmannMachine rbm, double[] v0Sample, double[] mean, double[] sample) {
        for(int i=0; i<rbm.getHiddenCount(); i++) {
            mean[i] = propUp(rbm, v0Sample, rbm.getLayer().getWeights()[i], rbm.getBiasH()[i]);
            sample[i] = rbm.binomial(1, mean[i]);
        }
    }

    public double propUp(RestrictedBoltzmannMachine rbm, double[] v, double[] w, double b) {
        double temp = 0.0;
        for(int j=0; j<rbm.getVisibleCount(); j++) {
            temp += w[j] * v[j];
        }
        temp += b;
        return RestrictedBoltzmannMachine.sigmoid(temp);
    }

    public void gibbsHVH(RestrictedBoltzmannMachine rbm, double[] sampleH0,
                         double[] meansNV, double[] samplesNV, double[] meansNH, double[] samplesNH) {
        sampleVH(rbm, sampleH0, meansNV, samplesNV);
        sampleHV(rbm, samplesNV, meansNH, samplesNH);
    }

    public void sampleVH(RestrictedBoltzmannMachine rbm, double[] sampleH0, double[] mean, double[] sample) {
        for(int i=0; i<rbm.getVisibleCount(); i++) {
            mean[i] = propDown(rbm, sampleH0, i, rbm.getBiasV()[i]);
            sample[i] = rbm.binomial(1, mean[i]);
        }
    }


    public double propDown(RestrictedBoltzmannMachine rbm, double[] h, int i, double b) {
        double temp = 0.0;
        for(int j=0; j<rbm.getHiddenCount(); j++) {
            temp += rbm.getLayer().getWeights()[j][i] * h[j];
        }
        temp += b;
        return RestrictedBoltzmannMachine.sigmoid(temp);
    }

}
