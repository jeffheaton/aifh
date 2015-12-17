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

namespace AIFH_Vol3_Core.Core.Selection
{
    /// <summary>
    ///     A search axis.
    /// </summary>
    public interface ISearchAxis
    {
        /// <summary>
        ///     Reset the axis back to the beginning.
        /// </summary>
        void Reset();

        /// <summary>
        ///     Advance a position in the search.
        /// </summary>
        /// <returns>True if there is more to read.</returns>
        bool Advance();

        /// <summary>
        ///     The current state of the axis.
        /// </summary>
        /// <returns>The current value of the axis.</returns>
        object CurrentState();

        /// <summary>
        ///     Sample a random value from the axis.
        /// </summary>
        /// <param name="rnd">Random number generator.</param>
        /// <returns>The value randomly picked.</returns>
        object Sample(IGenerateRandom rnd);
    }
}