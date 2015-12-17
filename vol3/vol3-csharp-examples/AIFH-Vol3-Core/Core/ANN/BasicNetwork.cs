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
using System.Collections.Generic;
using AIFH_Vol3.Core;
using AIFH_Vol3.Core.Learning;
using AIFH_Vol3_Core.Core.ANN.Randomize;
using AIFH_Vol3_Core.Core.Util;

namespace AIFH_Vol3_Core.Core.ANN
{
    /// <summary>
    ///     The base class for most feedforward networks in this book.  This includes deep and convolutional networks.
    ///     Layers must be added to the neural network and then a call to finalizeStructure makes the network ready for use.
    ///     After the call to finalizeStructure, layers can no longer be added to the NN.
    /// </summary>
    public class BasicNetwork : IRegressionAlgorithm, IClassificationAlgorithm
    {
        /// <summary>
        ///     The number of input neurons in this network.
        /// </summary>
        private int _inputCount;

        /// <summary>
        ///     The outputs from each of the neurons.
        /// </summary>
        private double[] _layerOutput;

        /// <summary>
        ///     The layers of the network.
        /// </summary>
        private readonly IList<ILayer> _layers = new List<ILayer>();

        /// <summary>
        ///     The sum of the layer, before the activation function is applied, producing the layerOutput.
        /// </summary>
        private double[] _layerSums;

        /// <summary>
        ///     True if the network is in training mode.  Some layers act differently while training (i.e. dropout).
        /// </summary>
        private bool _networkTraining;

        /// <summary>
        ///     The number of output neurons in this network.
        /// </summary>
        private int _outputCount;

        /// <summary>
        ///     The weights for a neural network.
        /// </summary>
        private double[] _weights;

        /// <summary>
        ///     The total number of neurons in the neural network.
        /// </summary>
        public int NeuronCount
        {
            get
            {
                var result = 0;
                foreach (var layer in _layers)
                {
                    result += layer.TotalCount;
                }
                return result;
            }
        }

        /// <summary>
        ///     The length of the array the network would encode to.
        /// </summary>
        public int EncodeLength
        {
            get { return _weights.Length; }
        }


        /// <summary>
        ///     The number of input neurons.
        /// </summary>
        public int InputCount
        {
            get { return _inputCount; }
            set { _inputCount = value; }
        }

        /// <summary>
        ///     The output for each layer.
        /// </summary>
        public double[] LayerOutput
        {
            get { return _layerOutput; }
        }

        /// <summary>
        ///     The number of output neurons.
        /// </summary>
        public int OutputCount
        {
            get { return _outputCount; }
            set { _outputCount = value; }
        }

        /// <summary>
        ///     The index of each layer in the weight and threshold array.
        /// </summary>
        public double[] Weights
        {
            get { return _weights; }
        }

        /// <summary>
        ///     The layer sums (before activation function applied).
        /// </summary>
        public double[] LayerSums
        {
            get { return _layerSums; }
        }

        /// <summary>
        ///     The layers of the neural network.
        /// </summary>
        public IList<ILayer> Layers
        {
            get { return _layers; }
        }

        /// <summary>
        ///     True, if the neural network is training.  Some layers (e.g. dropout) behave differently.
        /// </summary>
        public bool IsNetworkTraining
        {
            get { return _networkTraining; }
        }

        /// <summary>
        ///     Determine if the neural network is training.
        /// </summary>
        public bool NetworkTraining { set; get; }

        /// <summary>
        ///     Classify the specified input into a group.
        /// </summary>
        /// <param name="input">The input data.</param>
        /// <returns>The group the data was classified into.</returns>
        public int ComputeClassification(double[] input)
        {
            return ArrayUtil.IndexOfLargest(ComputeRegression(input));
        }

        /// <summary>
        ///     Compute the output for the specified input.
        /// </summary>
        /// <param name="input">The input.</param>
        /// <returns>The regression output.</returns>
        public double[] ComputeRegression(double[] input)
        {
            if (input.Length != InputCount)
            {
                throw new AIFHError("Invalid input count(" + input.Length + "), this network is designed for: "
                                    + InputCount);
            }
            var output = new double[OutputCount];
            Compute(input, output);
            return output;
        }

        /// <summary>
        ///     The long term memory for the algorithm.  This is usually weights or other coefficients.
        /// </summary>
        public double[] LongTermMemory
        {
            get { return _weights; }
        }

        /// <summary>
        ///     Calculate the output for the given input.
        /// </summary>
        /// <param name="input">The input.</param>
        /// <param name="output">Output will be placed here.</param>
        public void Compute(double[] input, double[] output)
        {
            ClearOutput();

            var sourceIndex = NeuronCount - _layers[0].TotalCount;

            Array.Copy(input, 0, _layerOutput, sourceIndex,
                _inputCount);

            for (var i = 1; i < _layers.Count; i++)
            {
                _layers[i].ComputeLayer();
            }


            Array.Copy(_layerOutput, 0, output, 0, _outputCount);
        }

        /// <summary>
        ///     Get the weight between the two layers.
        /// </summary>
        /// <param name="fromLayer">The from layer.</param>
        /// <param name="fromNeuron">The from neuron.</param>
        /// <param name="toNeuron">The to neuron.</param>
        /// <returns>The weight value.</returns>
        public double GetWeight(int fromLayer,
            int fromNeuron,
            int toNeuron)
        {
            ValidateNeuron(fromLayer, fromNeuron);
            ValidateNeuron(fromLayer + 1, toNeuron);
            var fromLayerNumber = _layers.Count - fromLayer - 1;
            var toLayerNumber = fromLayerNumber - 1;

            if (toLayerNumber < 0)
            {
                throw new AIFHError(
                    "The specified layer is not connected to another layer: "
                    + fromLayer);
            }

            var weightBaseIndex
                = _layers[fromLayer + 1].WeightIndex;
            var count = _layers[fromLayer].TotalCount;
            var weightIndex = weightBaseIndex + fromNeuron
                              + toNeuron*count;

            return _weights[weightIndex];
        }

        /// <summary>
        ///     Validate the the specified targetLayer and neuron are valid.
        /// </summary>
        /// <param name="targetLayer">The target layer.</param>
        /// <param name="neuron">The target neuron.</param>
        public void ValidateNeuron(int targetLayer, int neuron)
        {
            if ((targetLayer < 0) || (targetLayer >= _layers.Count))
            {
                throw new AIFHError("Invalid layer count: " + targetLayer);
            }

            if ((neuron < 0) || (neuron >= GetLayerTotalNeuronCount(targetLayer)))
            {
                throw new AIFHError("Invalid neuron number: " + neuron);
            }
        }

        /// <summary>
        ///     Get the total (including bias and context) neuron cont for a layer.
        /// </summary>
        /// <param name="l">The layer.</param>
        /// <returns>The count.</returns>
        public int GetLayerTotalNeuronCount(int l)
        {
            return _layers[l].TotalCount;
        }

        /// <summary>
        ///     Add a layer to the neural network.
        /// </summary>
        /// <param name="layer">The layer to be added to the neural network.</param>
        public void AddLayer(ILayer layer)
        {
            _layers.Add(layer);
        }

        /// <summary>
        ///     Finalize the structure of the neural network.  This must be called before any training or calculation can
        ///     be performed.  After this method is called layers can no longer be added to the neural network.
        /// </summary>
        public void FinalizeStructure()
        {
            var layerCount = _layers.Count;

            _inputCount = _layers[0].Count;
            _outputCount = _layers[layerCount - 1].Count;

            var counts = new TempStructureCounts();

            for (var i = _layers.Count - 1; i >= 0; i--)
            {
                var layer = _layers[i];

                layer.FinalizeStructure(this, i, counts);
            }

            _weights = new double[counts.WeightCount];
            _layerOutput = new double[counts.NeuronCount];
            _layerSums = new double[counts.NeuronCount];

            ClearOutput();
        }

        /// <summary>
        ///     Clear the outputs of each layer.
        /// </summary>
        public void ClearOutput()
        {
            // Clear all outputs to 0
            for (var i = 0; i < _layerOutput.Length; i++)
            {
                _layerOutput[i] = 0.0;
                _layerSums[i] = 0.0;
            }
            // Init the output arrays by filling in bias values
            var index = 0;
            for (var i = 0; i < _layers.Count; i++)
            {
                var layer = _layers[_layers.Count - 1 - i];
                index += layer.Count;
                if (layer.HasBias)
                {
                    _layerOutput[index++] = 1.0;
                }
            }
        }

        /// <summary>
        ///     Set the weight between the two specified neurons. The bias neuron is always
        ///     the last neuron on a layer.
        /// </summary>
        /// <param name="fromLayer">The from layer.</param>
        /// <param name="fromNeuron">The from neuron.</param>
        /// <param name="toNeuron">The to neuron.</param>
        /// <param name="value">The to value.</param>
        public void SetWeight(int fromLayer, int fromNeuron,
            int toNeuron, double value)
        {
            var fromLayerNumber = _layers.Count - fromLayer - 1;
            var toLayerNumber = fromLayerNumber - 1;

            if (toLayerNumber < 0)
            {
                throw new AIFHError(
                    "The specified layer is not connected to another layer: "
                    + fromLayer);
            }

            var weightBaseIndex
                = _layers[fromLayer + 1].WeightIndex;
            var count = _layers[fromLayer].TotalCount;

            var weightIndex = weightBaseIndex + fromNeuron
                              + toNeuron*count;

            Weights[weightIndex] = value;
        }

        /// <summary>
        ///     Randomize the neural network.
        /// </summary>
        public void Reset()
        {
            var random = new XaiverRandomizeNetwork();
            random.Randomize(this);
        }

        /// <summary>
        ///     Find the next layer in a neural network, given a layer.
        /// </summary>
        /// <param name="layer">The reference layer.</param>
        /// <returns>The next layer in the neural network.</returns>
        public ILayer GetNextLayer(ILayer layer)
        {
            var idx = _layers.IndexOf(layer);
            if (idx == -1)
            {
                throw new AIFHError("Can't find next layer for a layer that is not part of this network.");
            }
            if (idx >= _layers.Count)
            {
                throw new AIFHError("Can't find the next layer for the final layer in a network.");
            }
            return _layers[idx + 1];
        }

        /// <summary>
        ///     Find the previous layer in a neural network, given a layer.
        /// </summary>
        /// <param name="layer">The reference layer.</param>
        /// <returns>The previous layer in the neural network.</returns>
        public ILayer GetPreviousLayer(ILayer layer)
        {
            var idx = _layers.IndexOf(layer);
            if (idx == -1)
            {
                throw new AIFHError("Can't find previous layer for a layer that is not part of this network.");
            }
            if (idx == 0)
            {
                throw new AIFHError("Can't find the previous layer for the final layer in a network.");
            }
            return _layers[idx - 1];
        }
    }
}