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

namespace AIFH_Vol3.Core.Distance
{
    /// <summary>
    /// Chebyshev distance is the maximum absolute difference between any two vector elements.  This can be thought
    /// of as the number of spaces that a king chess piece must travel between two squares in a 2D dimension space.
    ///
    /// http://www.heatonresearch.com/wiki/Chebyshev_Distance
    /// </summary>
    public class ChebyshevDistance : AbstractDistance
    {
        /// <inheritdoc/>
        public override double Calculate(double[] position1, int pos1, double[] position2, int pos2, int length)
        {
            double result = 0;
            for (int i = 0; i < length; i++)
            {
                double d = Math.Abs(position1[pos1 + i] - position2[pos2 + i]);
                result = Math.Max(d, result);
            }
            return result;
        }

    }
}
