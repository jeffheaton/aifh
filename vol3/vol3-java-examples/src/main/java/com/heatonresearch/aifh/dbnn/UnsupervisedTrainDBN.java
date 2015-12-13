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

/**
 * Unsupervised training for DBNN's. This class trains a single layer at a time.
 */
public class UnsupervisedTrainDBN {

    /**
     * The network being trained.
     */
    private final DeepBeliefNetwork network;

    /**
     * The level bring trained.
     */
    private final int level;

    /**
     * The training cases.
     */
    private final double[][] trainingInput;

    /**
     * The learning rate.
     */
    private final double learningRate;

    /**
     * The number of cycles per iteration.
     */
    private final int k;

    /**
     * Construct a trainer for upsupervised training for the DBNN.
     * @param theNetwork The DBNN to train.
     * @param theLevel The level of the DBNN being trained.
     * @param theTrainingInput The training input cases.
     * @param theLearningRate The learning rate.
     * @param theK The number of cycles to use per iteration.
     */
    public UnsupervisedTrainDBN(DeepBeliefNetwork theNetwork, int theLevel, double[][] theTrainingInput,
                                double theLearningRate, int theK) {
        this.network = theNetwork;
        this.level = theLevel;
        this.trainingInput = theTrainingInput;
        this.learningRate = theLearningRate;
        this.k = theK;
    }

    /**
     * Perform one iteration of unsupervised training on the DBN.
     */
    public void iteration() {
        double[] layerInput = new double[0];
        int prevLayerInputSize;
        double[] prevLayerInput;


        for (final double[] aTrainingInput : this.trainingInput) {

            // Perform layer-wise sample up to the layer being trained.
            for (int l = 0; l <= this.level; l++) {

                if (l == 0) {
                    layerInput = new double[this.network.getInputCount()];
                    System.arraycopy(aTrainingInput, 0, layerInput, 0, this.network.getInputCount());
                } else {
                    // Get the previous input size, if we are on the first layer, this is the input count.
                    // Otherwise it is the input (visible) count from the previous layer.
                    if (l == 1) prevLayerInputSize = this.network.getInputCount();
                    else prevLayerInputSize = this.network.getLayers()[l - 1].getInputCount();

                    // Copy the previous layer's input to a new array.
                    prevLayerInput = new double[prevLayerInputSize];
                    System.arraycopy(layerInput, 0, prevLayerInput, 0, prevLayerInputSize);

                    // Construct an array to hold the current layer's input
                    layerInput = new double[this.network.getLayers()[l].getInputCount()];

                    // Sample this layer's hidden neuron values (h), given previous layer's input (v).
                    // The output goes into layerInput, which is used in the next layer.
                    this.network.getLayers()[l - 1].sampleHgivenV(prevLayerInput, layerInput);
                }
            }

            // Perform up-down algorithm.
            contrastiveDivergence(this.network.getRBMLayers()[this.level], layerInput, this.learningRate, this.k);
        }
    }

    /**
     * Perform contrastive divergence, also known as the up-down algorithm.
     * @param rbm The RBM to use.
     * @param input The input training pattern.
     * @param lr The learning rate.
     * @param k The number of cycles.
     */
    public void contrastiveDivergence(RestrictedBoltzmannMachine rbm, double[] input, double lr, int k) {
        // The positive gradient mean & samples (P) - Only for hidden (H)
        double[] meanPH = new double[rbm.getHiddenCount()];
        double[] samplePH = new double[rbm.getHiddenCount()];
        // The negative gradient mean & samples (N) - For both visible (V) & hidden (H)
        double[] meansNV = new double[rbm.getVisibleCount()];
        double[] samplesNV = new double[rbm.getVisibleCount()];
        double[] meansNH = new double[rbm.getHiddenCount()];
        double[] samplesNH = new double[rbm.getHiddenCount()];

        // Calculate (sample) meanPH and samplePH
        sampleHV(rbm, input, meanPH, samplePH);

        for(int step=0; step<k; step++) {
            if(step == 0) {
                gibbsHVH(rbm, samplePH, meansNV, samplesNV, meansNH, samplesNH);
            } else {
                gibbsHVH(rbm, samplesNH, meansNV, samplesNV, meansNH, samplesNH);
            }
        }

        // Adjust the weights, based on calculated mean values.
        // This uses the maximum likelihood learning rule.
        for(int i=0; i<rbm.getHiddenCount(); i++) {
            for(int j=0; j<rbm.getVisibleCount(); j++) {
                rbm.getLayer().getWeights()[i][j] += lr *(meanPH[i] * input[j] - meansNH[i] * samplesNV[j]) / input.length;
            }
            rbm.getBiasH()[i] += lr * (samplePH[i] - meansNH[i]) / input.length;
        }

        // Adjust the biases for learning.
        for(int i=0; i<rbm.getVisibleCount(); i++) {
            rbm.getBiasV()[i] += lr * (input[i] - samplesNV[i]) / input.length;
        }
    }

    /**
     * Sample the hidden neurons (output), given the visible (input).  Return the mean, and a sample, based on that
     * mean probability.
     * @param rbm The RBM to use.
     * @param v0Sample The input to the layer.
     * @param mean Output: mean value of each hidden neuron.
     * @param sample Output: sample, based on mean.
     */
    public void sampleHV(RestrictedBoltzmannMachine rbm, double[] v0Sample, double[] mean, double[] sample) {
        for(int i=0; i<rbm.getHiddenCount(); i++) {
            // Find the mean.
            mean[i] = propUp(rbm, v0Sample, rbm.getLayer().getWeights()[i], rbm.getBiasH()[i]);
            // Sample, based on that mean.
            sample[i] = rbm.binomial(1, mean[i]);
        }
    }

    /**
     * Estimate the mean of a hidden neuron in an RBM. Propagate upward part, from visible to hidden.
     * @param rbm The RBM to use.
     * @param v The input (v), visible neurons.
     * @param w The weights.
     * @param b The bias.
     * @return The mean.
     */
    public double propUp(RestrictedBoltzmannMachine rbm, double[] v, double[] w, double b) {
        double sum = 0.0;
        for(int j=0; j<rbm.getVisibleCount(); j++) {
            sum += w[j] * v[j];
        }
        sum += b;
        return RestrictedBoltzmannMachine.sigmoid(sum);
    }

    /**
     * Perform Gibbs sampling.  Hidden to visible to hidden.
     * @param rbm The RBM to use.
     * @param sampleH0 The hidden samples.
     * @param meansNV Output: means for the visible (v) neurons.
     * @param samplesNV Output: samples for the visible (v) neurons.
     * @param meansNH Output: means for the hidden (h) neurons.
     * @param samplesNH Output: samples for the hidden (h) neurons.
     */
    public void gibbsHVH(RestrictedBoltzmannMachine rbm, double[] sampleH0,
                         double[] meansNV, double[] samplesNV, double[] meansNH, double[] samplesNH) {
        sampleVH(rbm, sampleH0, meansNV, samplesNV);
        sampleHV(rbm, samplesNV, meansNH, samplesNH);
    }

    /**
     * Sample the visible (input), given the hidden neurons (output).  Return the mean, and a sample, based on that
     * mean probability.
     * @param rbm The RBM to use.
     * @param sampleH0 Hidden (h) samples.
     * @param mean Output: Visible (v) mean.
     * @param sample Output: Visible (v) sample.
     */
    public void sampleVH(RestrictedBoltzmannMachine rbm, double[] sampleH0, double[] mean, double[] sample) {
        for(int i=0; i<rbm.getVisibleCount(); i++) {
            mean[i] = propDown(rbm, sampleH0, i, rbm.getBiasV()[i]);
            sample[i] = rbm.binomial(1, mean[i]);
        }
    }

    /**
     * Estimate the mean of a visible neuron in an RBM. Propagate downward part, from hidden to visible.
     * @param rbm The RBM to use.
     * @param h The hidden neurons.
     * @param i The visible neuron to use.
     * @param b Bias value.
     * @return The estimated mean.
     */
    public double propDown(RestrictedBoltzmannMachine rbm, double[] h, int i, double b) {
        double sum = 0.0;
        for(int j=0; j<rbm.getHiddenCount(); j++) {
            sum += rbm.getLayer().getWeights()[j][i] * h[j];
        }
        sum += b;
        return RestrictedBoltzmannMachine.sigmoid(sum);
    }
}
