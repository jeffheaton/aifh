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

using System.Collections.Generic;

namespace AIFH_Vol3.Examples.Classic.Logic
{
    /// <summary>
    ///     A regular neuron, can function as either a hidden our output.
    /// </summary>
    public class RegularNeuron : INeuron
    {
        /// <summary>
        ///     The bias.
        /// </summary>
        private readonly double _bias;

        /// <summary>
        ///     The parents to this neuron.
        /// </summary>
        private readonly IList<Connection> _parents = new List<Connection>();

        /// <summary>
        ///     Construct the neuron.
        /// </summary>
        /// <param name="bias">The neuron's bias.</param>
        public RegularNeuron(double bias)
        {
            _bias = bias;
        }

        /// <summary>
        ///     The parent neurons.
        /// </summary>
        public IList<Connection> Parents
        {
            get { return _parents; }
        }

        /// <inheritdoc />
        public double Compute()
        {
            var sum = _bias;
            foreach (var c in _parents)
            {
                sum += c.Weight*c.Parent.Compute();
            }

            if (sum >= 0.5)
            {
                return 1;
            }
            return 0;
        }
    }
}