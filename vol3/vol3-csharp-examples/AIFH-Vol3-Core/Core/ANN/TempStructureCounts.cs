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
namespace AIFH_Vol3_Core.Core.ANN
{
    /// <summary>
    ///     Simple holder class that keeps the count of the neurons and weights as a neural network is built.  This class is
    ///     temporary and is only used briefly while the neural network structure is finalized.
    /// </summary>
    public class TempStructureCounts
    {
        /// <summary>
        ///     The number of neurons needed so far.
        /// </summary>
        public int NeuronCount { get; set; }

        /// <summary>
        ///     The number of weights needed so far.
        /// </summary>
        public int WeightCount { get; set; }

        /// <summary>
        ///     Add to the neuron count.
        /// </summary>
        /// <param name="i">The amount to add.</param>
        public void AddNeuronCount(int i)
        {
            NeuronCount += i;
        }

        /// <summary>
        ///     Add to the weight count.
        /// </summary>
        /// <param name="i">The amount to add.</param>
        public void AddWeightCount(int i)
        {
            WeightCount += i;
        }
    }
}