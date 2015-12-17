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
    public class TrainHopfieldHebbian
    {
        /// <summary>
        ///     The network to train.
        /// </summary>
        private readonly HopfieldNetwork _network;

        /// <summary>
        ///     The count of patterns.
        /// </summary>
        private int _patternCount;

        /// <summary>
        ///     The summation matrix.
        /// </summary>
        private readonly double[][] _sumMatrix;

        /// <summary>
        ///     Construct the trainer.
        /// </summary>
        /// <param name="theNetwork">The network to train.</param>
        public TrainHopfieldHebbian(HopfieldNetwork theNetwork)
        {
            _network = theNetwork;
            _sumMatrix = AIFH.Alloc2D<double>(_network.InputCount, _network.InputCount);
        }

        /// <summary>
        ///     Add a pattern to train.
        /// </summary>
        /// <param name="pattern">The pattern to train.</param>
        public void AddPattern(double[] pattern)
        {
            for (var i = 0; i < _sumMatrix.Length; i++)
            {
                for (var j = 0; j < _sumMatrix.Length; j++)
                {
                    if (i == j)
                    {
                        _sumMatrix[i][j] = 0;
                    }
                    else
                    {
                        _sumMatrix[i][j] += pattern[i]*pattern[j];
                    }
                }
            }
            _patternCount++;
        }

        /// <summary>
        ///     Learn the added patterns.
        /// </summary>
        public void Learn()
        {
            if (_patternCount == 0)
            {
                throw new AIFHError("Please add a pattern before learning.  Nothing to learn.");
            }

            for (var i = 0; i < _sumMatrix.Length; i++)
            {
                for (var j = 0; j < _sumMatrix.Length; j++)
                {
                    _network.SetWeight(i, j, _sumMatrix[i][j]/_patternCount);
                }
            }
        }
    }
}