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

using AIFH_Vol3.Core.Randomize;
using AIFH_Vol3_Core.Core.ANN.Activation;
using AIFH_Vol3_Core.Core.ANN.Train;

namespace AIFH_Vol3_Core.Core.ANN
{
    public interface ILayer
    {
        /// <summary>
        ///     The number of neurons, excluding bias neurons and context neurons. This is the number of neurons that
        ///     are directly fed from elsewhere.
        /// </summary>
        int Count { get; }

        /// <summary>
        ///     The number of neurons, including bias neurons and context neurons.
        /// </summary>
        int TotalCount { get; }

        /// <summary>
        ///     The activation/transfer function for this neuron.
        /// </summary>
        IActivationFunction Activation { get; }

        /// <summary>
        ///     The start of this layer's weights in the weight vector.
        /// </summary>
        int WeightIndex { get; }

        /// <summary>
        ///     The start of this layer's neurons in the neuron vector.
        /// </summary>
        int NeuronIndex { get; }

        /// <summary>
        ///     This layer's index in the layer stack.
        /// </summary>
        int LayerIndex { get; }

        /// <summary>
        ///     The owner of the neural network.
        /// </summary>
        BasicNetwork Owner { get; }

        /// <summary>
        ///     True if this neuron has bias.
        /// </summary>
        bool HasBias { get; }

        /// <summary>
        ///     The dimension conts, a single dimension network will return a 1D array with the count.
        /// </summary>
        int[] DimensionCounts { get; }

        /// <summary>
        ///     Get the number of weights in a single unit.  For  non-convolution layer, this is the total number of weights.
        ///     For a convolution network, this is the number of weights per filter.
        /// </summary>
        int WeightDepthUnit { get; }

        /// <summary>
        ///     Get the number of neurons in a single unit.  For non-convolution layers, this is the total number of neurons in
        ///     this layer.For convolution networks this is the number of neurons per filter.
        /// </summary>
        int NeuronDepthUnit { get; }

        /// <summary>
        ///     Finalize the structure of this layer.
        /// </summary>
        /// <param name="theOwner">The owning neural network.</param>
        /// <param name="theLayerIndex">The zero-based index of this layer.</param>
        /// <param name="counts">The counts structure, used to track the counts of neurons and weights.</param>
        void FinalizeStructure(BasicNetwork theOwner, int theLayerIndex,
            TempStructureCounts counts);

        /// <summary>
        ///     Compute this layer.
        /// </summary>
        void ComputeLayer();

        /// <summary>
        ///     Compute the gradients for this layer.
        /// </summary>
        /// <param name="calc">The gradient calculation utility.</param>
        void ComputeGradient(GradientCalc calc);

        /// <summary>
        ///     Notification that a training batch is beginning.
        /// </summary>
        /// <param name="rnd">A random number generator, from the trainer.</param>
        void TrainingBatch(IGenerateRandom rnd);

        /// <summary>
        ///     Determine if a neuron is active.
        /// </summary>
        /// <param name="i">The neuron to check.</param>
        /// <returns>True of that neuron is active.</returns>
        bool IsActive(int i);
    }
}