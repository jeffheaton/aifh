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

namespace AIFH_Vol3_Core.Core.ANN.Randomize
{
    /// <summary>
    ///     Neural network randomizer that simply assigns each weight and bias to a uniform random number between a high
    ///     and low range.
    /// </summary>
    public class RangeRandomizeNetwork : AbstractRandomizeNetwork
    {
        /// <summary>
        ///     The high end of the range.
        /// </summary>
        private readonly double _high;

        /// <summary>
        ///     The low end of the range.
        /// </summary>
        private readonly double _low;

        /// <summary>
        ///     Construct the range randomizer.
        /// </summary>
        /// <param name="theLow">The low end of the range.</param>
        /// <param name="theHigh">The high end of the range.</param>
        public RangeRandomizeNetwork(double theLow, double theHigh)
        {
            _low = theLow;
            _high = theHigh;
        }

        /// <summary>
        ///     Create a range randomizer between -1 and 1.
        /// </summary>
        public RangeRandomizeNetwork() : this(-1, 1)
        {
        }

        /// <inheritdoc />
        public override void Randomize(BasicNetwork network)
        {
            for (var i = 0; i < network.Weights.Length; i++)
            {
                network.Weights[i] = Rnd.NextDouble(_low, _high);
            }
        }
    }
}