// Artificial Intelligence for Humans
// Volume 3: Deep Learning and Neural Networks
// C# Version
// http://www.aifh.org
// http://www.jeffheaton.com
//
// Code repository:
// https://github.com/jeffheaton/aifh
//
// Copyright 2015 by Jeff Heaton
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// For more information on Heaton Research copyrights, licenses
// and trademarks visit:
// http://www.heatonresearch.com/copyright
//

using System;

namespace AIFH_Vol3_Core.Core.DBNN
{
    /// <summary>
    ///     Unsupervised training for DBNN's. This class trains a single layer at a time.
    /// </summary>
    public class UnsupervisedTrainDBN
    {
        /// <summary>
        ///     The number of cycles per iteration.
        /// </summary>
        private readonly int _k;

        /// <summary>
        ///     The learning rate.
        /// </summary>
        private readonly double _learningRate;

        /// <summary>
        ///     The level being trained.
        /// </summary>
        private readonly int _level;

        /// <summary>
        ///     The network being trained.
        /// </summary>
        private readonly DeepBeliefNetwork _network;

        /// <summary>
        ///     The training cases.
        /// </summary>
        private readonly double[][] _trainingInput;

        /// <summary>
        ///     Construct a trainer for upsupervised training for the DBNN.
        /// </summary>
        /// <param name="theNetwork">The DBNN to train.</param>
        /// <param name="theLevel">The level of the DBNN being trained.</param>
        /// <param name="theTrainingInput">The training input cases.</param>
        /// <param name="theLearningRate">The learning rate.</param>
        /// <param name="theK">The number of cycles to use per iteration.</param>
        public UnsupervisedTrainDBN(DeepBeliefNetwork theNetwork, int theLevel, double[][] theTrainingInput,
            double theLearningRate, int theK)
        {
            _network = theNetwork;
            _level = theLevel;
            _trainingInput = theTrainingInput;
            _learningRate = theLearningRate;
            _k = theK;
        }

        /// <summary>
        ///     Perform one iteration of unsupervised training on the DBN.
        /// </summary>
        public void Iteration()
        {
            var layerInput = new double[0];
            int prevLayerInputSize;
            double[] prevLayerInput;


            foreach (var aTrainingInput in _trainingInput)
            {
                // Perform layer-wise sample up to the layer being trained.
                for (var l = 0; l <= _level; l++)
                {
                    if (l == 0)
                    {
                        layerInput = new double[_network.InputCount];
                        Array.Copy(aTrainingInput, layerInput, _network.InputCount);
                    }
                    else
                    {
                        // Get the previous input size, if we are on the first layer, this is the input count.
                        // Otherwise it is the input (visible) count from the previous layer.
                        if (l == 1) prevLayerInputSize = _network.InputCount;
                        else prevLayerInputSize = _network.Layers[l - 1].InputCount;

                        // Copy the previous layer's input to a new array.
                        prevLayerInput = new double[prevLayerInputSize];
                        Array.Copy(layerInput, prevLayerInput, prevLayerInputSize);

                        // Construct an array to hold the current layer's input
                        layerInput = new double[_network.Layers[l].InputCount];

                        // Sample this layer's hidden neuron values (h), given previous layer's input (v).
                        // The output goes into layerInput, which is used in the next layer.
                        _network.Layers[l - 1].SampleHgivenV(prevLayerInput, layerInput);
                    }
                }

                // Perform up-down algorithm.
                ContrastiveDivergence(_network.RBMLayers[_level], layerInput, _learningRate, _k);
            }
        }

        /// <summary>
        ///     Perform contrastive divergence, also known as the up-down algorithm.
        /// </summary>
        /// <param name="rbm">The RBM to use.</param>
        /// <param name="input">The input training pattern.</param>
        /// <param name="lr">The learning rate.</param>
        /// <param name="k">The number of cycles.</param>
        public void ContrastiveDivergence(RestrictedBoltzmannMachine rbm, double[] input, double lr, int k)
        {
            // The positive gradient mean & samples (P) - Only for hidden (H)
            var meanPH = new double[rbm.HiddenCount];
            var samplePH = new double[rbm.HiddenCount];
            // The negative gradient mean & samples (N) - For both visible (V) & hidden (H)
            var meansNV = new double[rbm.VisibleCount];
            var samplesNV = new double[rbm.VisibleCount];
            var meansNH = new double[rbm.HiddenCount];
            var samplesNH = new double[rbm.HiddenCount];

            // Calculate (sample) meanPH and samplePH
            SampleHV(rbm, input, meanPH, samplePH);

            for (var step = 0; step < k; step++)
            {
                if (step == 0)
                {
                    GibbsHVH(rbm, samplePH, meansNV, samplesNV, meansNH, samplesNH);
                }
                else
                {
                    GibbsHVH(rbm, samplesNH, meansNV, samplesNV, meansNH, samplesNH);
                }
            }

            // Adjust the weights, based on calculated mean values.
            // This uses the maximum likelihood learning rule.
            for (var i = 0; i < rbm.HiddenCount; i++)
            {
                for (var j = 0; j < rbm.VisibleCount; j++)
                {
                    rbm.Layer.Weights[i][j] += lr*(meanPH[i]*input[j] - meansNH[i]*samplesNV[j])/input.Length;
                }
                rbm.BiasH[i] += lr*(samplePH[i] - meansNH[i])/input.Length;
            }

            // Adjust the biases for learning.
            for (var i = 0; i < rbm.VisibleCount; i++)
            {
                rbm.BiasV[i] += lr*(input[i] - samplesNV[i])/input.Length;
            }
        }

        /// <summary>
        ///     Sample the hidden neurons (output), given the visible (input).  Return the mean, and a sample, based on that
        ///     mean probability.
        /// </summary>
        /// <param name="rbm">The RBM to use.</param>
        /// <param name="v0Sample">The input to the layer.</param>
        /// <param name="mean">Output: mean value of each hidden neuron.</param>
        /// <param name="sample">Output: sample, based on mean.</param>
        public void SampleHV(RestrictedBoltzmannMachine rbm, double[] v0Sample, double[] mean, double[] sample)
        {
            for (var i = 0; i < rbm.HiddenCount; i++)
            {
                // Find the mean.
                mean[i] = PropUp(rbm, v0Sample, rbm.Layer.Weights[i], rbm.BiasH[i]);
                // Sample, based on that mean.
                sample[i] = rbm.binomial(1, mean[i]);
            }
        }

        /// <summary>
        ///     Estimate the mean of a hidden neuron in an RBM. Propagate upward part, from visible to hidden.
        /// </summary>
        /// <param name="rbm">The RBM to use.</param>
        /// <param name="v">The input (v), visible neurons.</param>
        /// <param name="w">The weights.</param>
        /// <param name="b">The bias.</param>
        /// <returns>The mean.</returns>
        public double PropUp(RestrictedBoltzmannMachine rbm, double[] v, double[] w, double b)
        {
            var sum = 0.0;
            for (var j = 0; j < rbm.VisibleCount; j++)
            {
                sum += w[j]*v[j];
            }
            sum += b;
            return RestrictedBoltzmannMachine.Sigmoid(sum);
        }

        /// <summary>
        ///     Perform Gibbs sampling.  Hidden to visible to hidden.
        /// </summary>
        /// <param name="rbm">The RBM to use.</param>
        /// <param name="sampleH0">The hidden samples.</param>
        /// <param name="meansNV">Output: means for the visible (v) neurons.</param>
        /// <param name="samplesNV">Output: samples for the visible (v) neurons.</param>
        /// <param name="meansNH">Output: means for the hidden (h) neurons.</param>
        /// <param name="samplesNH">Output: samples for the hidden (h) neurons.</param>
        public void GibbsHVH(RestrictedBoltzmannMachine rbm, double[] sampleH0,
            double[] meansNV, double[] samplesNV, double[] meansNH, double[] samplesNH)
        {
            SampleVH(rbm, sampleH0, meansNV, samplesNV);
            SampleHV(rbm, samplesNV, meansNH, samplesNH);
        }

        /// <summary>
        ///     Sample the visible (input), given the hidden neurons (output).  Return the mean, and a sample, based on that
        ///     mean probability.
        /// </summary>
        /// <param name="rbm">The RBM to use.</param>
        /// <param name="sampleH0">Hidden (h) samples.</param>
        /// <param name="mean">Output: Visible (v) mean.</param>
        /// <param name="sample">Output: Visible (v) sample.</param>
        public void SampleVH(RestrictedBoltzmannMachine rbm, double[] sampleH0, double[] mean, double[] sample)
        {
            for (var i = 0; i < rbm.VisibleCount; i++)
            {
                mean[i] = PropDown(rbm, sampleH0, i, rbm.BiasV[i]);
                sample[i] = rbm.binomial(1, mean[i]);
            }
        }

        /// <summary>
        ///     Estimate the mean of a visible neuron in an RBM. Propagate downward part, from hidden to visible.
        /// </summary>
        /// <param name="rbm">The RBM to use.</param>
        /// <param name="h">The hidden neurons.</param>
        /// <param name="i">The visible neuron to use.</param>
        /// <param name="b">Bias value.</param>
        /// <returns>The estimated mean.</returns>
        public double PropDown(RestrictedBoltzmannMachine rbm, double[] h, int i, double b)
        {
            var sum = 0.0;
            for (var j = 0; j < rbm.HiddenCount; j++)
            {
                sum += rbm.Layer.Weights[j][i]*h[j];
            }
            sum += b;
            return RestrictedBoltzmannMachine.Sigmoid(sum);
        }
    }
}