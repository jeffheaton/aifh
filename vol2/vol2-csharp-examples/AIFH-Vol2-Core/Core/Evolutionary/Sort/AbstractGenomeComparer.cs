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

namespace AIFH_Vol2.Core.Evolutionary.Sort
{
    /// <summary>
    ///     Provides base functionality for comparing genomes. Specifically the ability
    ///     to add bonuses and penalties.
    /// </summary>
    [Serializable]
    public abstract class AbstractGenomeComparer : IGenomeComparer
    {
        /// <inheritdoc />
        public double ApplyBonus(double value, double bonus)
        {
            double amount = value*bonus;
            if (ShouldMinimize)
            {
                return value - amount;
            }
            return value + amount;
        }

        /// <inheritdoc />
        public double ApplyPenalty(double value, double bonus)
        {
            double amount = value*bonus;
            if (!ShouldMinimize)
            {
                return value - amount;
            }
            return value + amount;
        }

        /// <inheritdoc />
        public bool IsBetterThan(double d1, double d2)
        {
            if (ShouldMinimize)
            {
                return d1 < d2;
            }
            return d1 > d2;
        }


        public abstract bool IsBetterThan(IGenome genome1, IGenome genome2);

        public abstract bool ShouldMinimize { get; }

        /// <summary>
        ///     Compare two genomes.
        /// </summary>
        /// <param name="x">The first genome.</param>
        /// <param name="y">The second genome.</param>
        /// <returns>
        ///     0 if equal, &lt;0 if x is less,&gt;0 if y is less.
        /// </returns>
        public abstract int Compare(IGenome x, IGenome y);
    }
}
