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
using AIFH_Vol3.Core;
using AIFH_Vol3.Core.Learning;
using AIFH_Vol3.Core.Randomize;

namespace AIFH_Vol3_Core.Core.DBNN
{
    /// <summary>
    ///     A deep belief neural network.
    ///     References:
    ///     http://deeplearning.net/software/theano/
    ///     https://github.com/yusugomori/DeepLearning
    ///     http://en.wikipedia.org/wiki/Deep_learning
    /// </summary>
    public class DeepBeliefNetwork : IRegressionAlgorithm
    {
        /// <summary>
        ///     The hidden layers of the neural network.
        /// </summary>
        private readonly HiddenLayer[] _layers;

        /// <summary>
        ///     The output layer for the neural network.
        /// </summary>
        private readonly DeepLayer _outputLayer;

        /// <summary>
        ///     The restricted boltzmann machines for the neural network, one for leach layer.
        /// </summary>
        private readonly RestrictedBoltzmannMachine[] _rbm;

        /// <summary>
        ///     Construct a deep belief neural network.
        /// </summary>
        /// <param name="inputCount">The input count.</param>
        /// <param name="hidden">The counts for the hidden layers.</param>
        /// <param name="outputCount">The output neuron count.</param>
        public DeepBeliefNetwork(int inputCount, int[] hidden, int outputCount)
        {
            int inputSize;

            _layers = new HiddenLayer[hidden.Length];
            _rbm = new RestrictedBoltzmannMachine[hidden.Length];

            for (var i = 0; i < _rbm.Length; i++)
            {
                if (i == 0)
                {
                    inputSize = inputCount;
                }
                else
                {
                    inputSize = hidden[i - 1];
                }

                _layers[i] = new HiddenLayer(this, inputSize, hidden[i]);

                _rbm[i] = new RestrictedBoltzmannMachine(_layers[i]);
            }

            _outputLayer = new DeepLayer(this, hidden[_layers.Length - 1], outputCount);
            Random = new MersenneTwisterGenerateRandom();
        }

        /// <summary>
        ///     The random number generator to use.
        /// </summary>
        public IGenerateRandom Random { get; set; }

        /// <summary>
        ///     The layers of the neural network.
        /// </summary>
        /// <returns></returns>
        public HiddenLayer[] Layers
        {
            get { return _layers; }
        }

        /// <summary>
        ///     The restricted Boltzmann machines.
        /// </summary>
        public RestrictedBoltzmannMachine[] RBMLayers
        {
            get { return _rbm; }
        }

        /// <summary>
        ///     The input count.
        /// </summary>
        public int InputCount
        {
            get { return _layers[0].InputCount; }
        }

        /// <summary>
        ///     The output (logistic) layer.
        /// </summary>
        public DeepLayer LogLayer
        {
            get { return _outputLayer; }
        }


        /// <summary>
        ///     The number of output neurons.
        /// </summary>
        public int OutputCount
        {
            get { return _outputLayer.OutputCount; }
        }

        /// <summary>
        ///     Classify the input data into the list of probabilities of each class.
        /// </summary>
        /// <param name="input">The input.</param>
        /// <returns>An array that contains the probabilities of each class.</returns>
        public double[] ComputeRegression(double[] input)
        {
            var result = new double[OutputCount];
            var layerInput = new double[0];
            var prevLayerInput = new double[InputCount];

            Array.Copy(input, prevLayerInput, InputCount);

            double output;

            for (var i = 0; i < _layers.Length; i++)
            {
                layerInput = new double[_layers[i].OutputCount];

                for (var k = 0; k < _layers[i].OutputCount; k++)
                {
                    output = 0.0;

                    for (var j = 0; j < _layers[i].InputCount; j++)
                    {
                        output += _layers[i].Weights[k][j]*prevLayerInput[j];
                    }
                    output += _layers[i].Bias[k];
                    layerInput[k] = Sigmoid(output);
                }

                if (i < _layers.Length - 1)
                {
                    prevLayerInput = new double[_layers[i].OutputCount];
                    Array.Copy(layerInput, 0, prevLayerInput, 0, _layers[i].OutputCount);
                }
            }

            for (var i = 0; i < _outputLayer.OutputCount; i++)
            {
                result[i] = 0;
                for (var j = 0; j < _outputLayer.InputCount; j++)
                {
                    result[i] += _outputLayer.Weights[i][j]*layerInput[j];
                }
                result[i] += _outputLayer.Bias[i];
            }

            _outputLayer.Softmax(result);
            return result;
        }

        /// <inheritdoc />
        public double[] LongTermMemory
        {
            get { throw new AIFHError("Can't access DBM memory as array."); }
        }

        /// <summary>
        ///     Randomize the weights of the neural network.
        /// </summary>
        public void Reset()
        {
            for (var i = 0; i < _rbm.Length; i++)
            {
                var layer = _layers[i];

                var a = 1.0/layer.InputCount;

                for (var j = 0; j < layer.OutputCount; j++)
                {
                    for (var k = 0; k < layer.InputCount; k++)
                    {
                        layer.Weights[j][k] = Random.NextDouble(-a, a);
                    }
                }
            }
        }

        /// <summary>
        ///     The sigmoid/logistic function, used by the output layer.
        /// </summary>
        /// <param name="x">The input.</param>
        /// <returns>The output.</returns>
        public static double Sigmoid(double x)
        {
            return 1.0/(1.0 + Math.Exp(-x));
        }
    }
}