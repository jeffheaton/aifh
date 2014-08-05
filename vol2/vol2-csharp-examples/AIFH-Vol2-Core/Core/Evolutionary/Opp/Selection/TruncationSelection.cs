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
    /// Truncation selection chooses a random genome from the top genomes in the
    /// population. A percent determines how large this group of top genomes is.
    /// 
    /// http://en.wikipedia.org/wiki/Truncation_selection
    /// </summary>
    [Serializable]
    public class TruncationSelection
    {
        /// <summary>
        /// The trainer.
        /// </summary>
        private IEvolutionaryAlgorithm _trainer;

        /// <summary>
        /// The percent to select from.
        /// </summary>
        private double _percent;

        /// <summary>
        /// Construct the truncation selector.
        /// </summary>
        /// <param name="theTrainer">The trainer.</param>
        /// <param name="thePercent">The top percent to select from.</param>
        public TruncationSelection(IEvolutionaryAlgorithm theTrainer,
                                   double thePercent)
        {
            _trainer = theTrainer;
            _percent = thePercent;
        }

        /// <inheritdoc/>
        public int PerformSelection(IGenerateRandom rnd, ISpecies species)
        {
            int top = Math.Max((int)(species.Members.Count * _percent),
                    1);
            return rnd.NextInt(top);
        }

        /// <inheritdoc/>
        public int PerformAntiSelection(IGenerateRandom rnd, ISpecies species)
        {
            return species.Members.Count - PerformSelection(rnd, species);
        }

        /// <inheritdoc/>
        public IEvolutionaryAlgorithm Trainer
        {
            get
            {
                return _trainer;
            }
        }
    }
}
