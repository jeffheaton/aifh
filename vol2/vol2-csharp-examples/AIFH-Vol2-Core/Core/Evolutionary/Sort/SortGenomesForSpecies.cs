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
using AIFH_Vol2.Core.Evolutionary.Train;
using System.Collections.Generic;

namespace AIFH_Vol2.Core.Evolutionary.Sort
{
    /// <summary>
    ///     Sort the gnomes for species.  Sort first by score, second by birth generation.
    ///     This favors younger genomes if scores are equal.
    /// </summary>
    public class SortGenomesForSpecies : IComparer<IGenome>
    {
        /// <summary>
        ///     The trainer.
        /// </summary>
        private readonly IEvolutionaryAlgorithm _train;

        /// <summary>
        ///     Construct the comparator.
        /// </summary>
        /// <param name="theTrain">The trainer.</param>
        public SortGenomesForSpecies(IEvolutionaryAlgorithm theTrain)
        {
            _train = theTrain;
        }

        /// <inheritdoc />
        public int Compare(IGenome g1, IGenome g2)
        {
            int result = _train.SelectionComparer.Compare(g1, g2);

            if (result != 0)
            {
                return result;
            }

            return g2.BirthGeneration - g1.BirthGeneration;
        }
    }
}
