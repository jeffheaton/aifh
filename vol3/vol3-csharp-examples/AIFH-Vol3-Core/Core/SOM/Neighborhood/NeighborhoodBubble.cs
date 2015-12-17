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

namespace AIFH_Vol3_Core.Core.SOM.Neighborhood
{
    /// <summary>
    ///     A neighborhood function that uses a simple bubble. A radius is defined, and
    ///     any neuron that is plus or minus that width from the winning neuron will be
    ///     updated as a result of training.
    /// </summary>
    public class NeighborhoodBubble : INeighborhoodFunction
    {
        /// <summary>
        ///     Create a bubble neighborhood function that will return 1.0 (full update)
        ///     for any neuron that is plus or minus the width distance from the winning
        ///     neuron.
        /// </summary>
        /// <param name="radius">
        ///     The width of the bubble, this is the distance that the neuron
        ///     can be from the winning neuron.The true width, across the
        ///     bubble, is actually two times this parameter.
        /// </param>
        public NeighborhoodBubble(int radius)
        {
            Radius = radius;
        }

        /// <summary>
        ///     The radius of the bubble.
        /// </summary>
        public double Radius { get; set; }

        /// <summary>
        ///     Determine how much the current neuron should be affected by training
        ///     based on its proximity to the winning neuron.
        /// </summary>
        /// <param name="currentNeuron">THe current neuron being evaluated.</param>
        /// <param name="bestNeuron">The winning neuron.</param>
        /// <returns>The ratio for this neuron's adjustment.</returns>
        public double Function(int currentNeuron, int bestNeuron)
        {
            var distance = Math.Abs(bestNeuron - currentNeuron);
            if (distance <= Radius)
            {
                return 1.0;
            }
            return 0.0;
        }
    }
}