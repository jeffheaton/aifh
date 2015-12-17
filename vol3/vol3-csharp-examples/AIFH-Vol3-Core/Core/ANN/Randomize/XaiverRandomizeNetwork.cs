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

namespace AIFH_Vol3_Core.Core.ANN.Randomize
{
    /// <summary>
    ///     The Xaiver initialization (aka Glorot) weight initialization.  A very good weight initialization method that
    ///     provides very efficient training and relatively consistent results.As described by the following paper.
    ///     Glorot, X., & Bengio, Y. (2010). Understanding the difficulty of training deep feedforward neural networks.
    ///     In International conference on artificial intelligence and statistics(pp. 249-256).
    /// </summary>
    public class XaiverRandomizeNetwork : AbstractRandomizeNetwork
    {
        /// <summary>
        ///     The Xaiver initialization works layer by layer.
        /// </summary>
        /// <param name="network">The network.</param>
        /// <param name="fromLayer">The source layer.</param>
        private void RandomizeLayer(BasicNetwork network, int fromLayer)
        {
            var fromCount = network.GetLayerTotalNeuronCount(fromLayer);
            var toCount = network.Layers[fromLayer + 1].Count;

            for (var fromNeuron = 0; fromNeuron < fromCount; fromNeuron++)
            {
                for (var toNeuron = 0; toNeuron < toCount; toNeuron++)
                {
                    var sigma = Math.Sqrt(2.0/(fromCount + toCount));
                    var w = Rnd.NextGaussian()*sigma;
                    network.SetWeight(fromLayer, fromNeuron, toNeuron, w);
                }
            }
        }

        /// <inheritdoc />
        public override void Randomize(BasicNetwork network)
        {
            for (var i = 0; i < network.Layers.Count - 1; i++)
            {
                RandomizeLayer(network, i);
            }
        }
    }
}