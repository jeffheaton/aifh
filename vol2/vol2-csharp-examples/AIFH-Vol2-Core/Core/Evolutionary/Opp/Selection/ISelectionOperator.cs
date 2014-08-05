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
using AIFH_Vol2.Core.Evolutionary.Species;
using AIFH_Vol2.Core.Evolutionary.Train;
using AIFH_Vol2.Core.Randomize;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol2.Core.Evolutionary.Opp.Selection
{
    /// <summary>
    /// Provides the interface to a selection operator. This allows genomes to be
    /// selected for offspring production or elimination.
    /// </summary>
    public interface ISelectionOperator
    {
        /// <summary>
        /// Selects an fit genome.
        /// </summary>
        /// <param name="rnd">A random number generator.</param>
        /// <param name="species">The species to select the genome from.</param>
        /// <returns>The selected genome.</returns>
        int PerformSelection(IGenerateRandom rnd, ISpecies species);

        /// <summary>
        /// Selects an unfit genome. 
        /// </summary>
        /// <param name="rnd">A random number generator.</param>
        /// <param name="species">The species to select the genome from.</param>
        /// <returns>The selected genome.</returns>
        int PerformAntiSelection(IGenerateRandom rnd, ISpecies species);

        /// <summary>
        /// The trainer being used.
        /// </summary>
        IEvolutionaryAlgorithm Trainer { get; }
    }
}
