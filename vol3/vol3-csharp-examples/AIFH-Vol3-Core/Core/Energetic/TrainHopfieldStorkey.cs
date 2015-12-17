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

using AIFH_Vol3.Core;

namespace AIFH_Vol3_Core.Core.Energetic
{
    /// <summary>
    ///     Train the Hopfield network using a Hebbian algorithm.
    ///     For more info: https://en.wikipedia.org/wiki/Hopfield_network
    /// </summary>
    public class TrainHopfieldStorkey
    {
        /// <summary>
        ///     The network to train.
        /// </summary>
        private readonly HopfieldNetwork _network;

        // The summation matrix.
        /// <summary>
        /// </summary>
        private readonly double[][] _sumMatrix;

        /// <summary>
        ///     Construct the trainer.
        /// </summary>
        /// <param name="theNetwork">The network to train.</param>
        public TrainHopfieldStorkey(HopfieldNetwork theNetwork)
        {
            _network = theNetwork;
            _sumMatrix = AIFH.Alloc2D<double>(_network.InputCount, _network.InputCount);
        }

        /// <summary>
        ///     Calculate the local field needed by training.
        /// </summary>
        /// <param name="i">The neuron.</param>
        /// <param name="pattern">The pattern.</param>
        /// <returns>The local field value.</returns>
        private double CalculateLocalField(int i, double[] pattern)
        {
            double sum = 0;
            for (var k = 0; k < _network.InputCount; k++)
            {
                if (k != i)
                {
                    sum += _network.GetWeight(i, k)*pattern[k];
                }
            }
            return sum;
        }

        /// <summary>
        ///     Add a pattern for training.
        /// </summary>
        /// <param name="pattern">The pattern to add.</param>
        public void AddPattern(double[] pattern)
        {
            for (var i = 0; i < _sumMatrix.Length; i++)
            {
                for (var j = 0; j < _sumMatrix.Length; j++)
                {
                    _sumMatrix[i][j] = 0;
                }
            }

            double n = _network.InputCount;
            for (var i = 0; i < _sumMatrix.Length; i++)
            {
                for (var j = 0; j < _sumMatrix.Length; j++)
                {
                    var t1 = pattern[i]*pattern[j]/n;
                    var t2 = pattern[i]*CalculateLocalField(j, pattern)/n;
                    var t3 = pattern[j]*CalculateLocalField(i, pattern)/n;
                    var d = t1 - t2 - t3;
                    _sumMatrix[i][j] += d;
                }
            }

            for (var i = 0; i < _sumMatrix.Length; i++)
            {
                for (var j = 0; j < _sumMatrix.Length; j++)
                {
                    _network.SetWeight(i, j, _network.GetWeight(i, j) + _sumMatrix[i][j]);
                }
            }
        }
    }
}