// Artificial Intelligence for Humans
// Volume 2: Nature-Inspired Algorithms
// C# Version
// http://www.aifh.org
// http://www.jeffheaton.com
//
// Code repository:
// https://github.com/jeffheaton/aifh
//
// Copyright 2014 by Jeff Heaton
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
using AIFH_Vol2.Core.Evolutionary.Genome;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol2.Core.Evolutionary.Sort
{
    /// <summary>
    ///     Defines methods for comparing genomes. Also provides methods to apply bonuses
    ///     and penalties.
    /// </summary>
    public interface IGenomeComparer : IComparer<IGenome>
    {
        /// <summary>
        ///     Returns true if the score should be minimized.
        /// </summary>
        bool ShouldMinimize { get; }

        /// <summary>
        ///     Apply a bonus, this is a simple percent that is applied in the direction
        ///     specified by the "should minimize" property of the score function.
        /// </summary>
        /// <param name="value">The current value.</param>
        /// <param name="bonus">The bonus.</param>
        /// <returns>The resulting value.</returns>
        double ApplyBonus(double value, double bonus);

        /// <summary>
        ///     Apply a penalty, this is a simple percent that is applied in the
        ///     direction specified by the "should minimize" property of the score
        ///     function.
        /// </summary>
        /// <param name="value">The current value.</param>
        /// <param name="bonus">The penalty.</param>
        /// <returns>The resulting value.</returns>
        double ApplyPenalty(double value, double bonus);

        /// <summary>
        ///     Determine if one score is better than the other.
        /// </summary>
        /// <param name="d1">The first score to compare.</param>
        /// <param name="d2">The second score to compare.</param>
        /// <returns>True if d1 is better than d2.</returns>
        bool IsBetterThan(double d1, double d2);

        /// <summary>
        ///     Determine if one genome is better than the other genome.
        /// </summary>
        /// <param name="genome1">The first genome.</param>
        /// <param name="genome2">The second genome.</param>
        /// <returns>True, if genome1 is better than genome2.</returns>
        bool IsBetterThan(IGenome genome1, IGenome genome2);
    }
}
