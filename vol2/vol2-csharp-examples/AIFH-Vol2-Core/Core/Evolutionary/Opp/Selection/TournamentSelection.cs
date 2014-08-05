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
    /// Tournament select can be used to select a fit (or unfit) genome from a
    /// species. The selection is run a set number of rounds. Each round two random
    /// participants are chosen. The more fit participant continues to the next
    /// round.
    /// 
    /// http://en.wikipedia.org/wiki/Tournament_selection
    /// </summary>
    [Serializable]
    public class TournamentSelection : ISelectionOperator
    {
        /**
         * The trainer being used.
         */
        public IEvolutionaryAlgorithm Trainer { get; set; }

        /**
         * The number of rounds.
         */
        public int Rounds { get; set; }

        /**
         * Construct a tournament selection.
         *
         * @param theTrainer The trainer.
         * @param theRounds  The number of rounds to use.
         */
        public TournamentSelection(IEvolutionaryAlgorithm theTrainer,
                                   int theRounds)
        {
            Trainer = theTrainer;
            Rounds = theRounds;
        }

        /// <inheritdoc/>
        public int PerformAntiSelection(IGenerateRandom rnd, ISpecies species)
        {
            int worstIndex = rnd.NextInt(species.Members.Count);
            IGenome worst = species.Members[worstIndex];
            BasicEA.CalculateScoreAdjustment(worst,
                    Trainer.ScoreAdjusters);

            for (int i = 0; i < this.Rounds; i++)
            {
                int competitorIndex = rnd.NextInt(species.Members.Count);
                IGenome competitor = species.Members[competitorIndex];

                // force an invalid genome to lose
                if (double.IsInfinity(competitor.AdjustedScore)
                        || double.IsNaN(competitor.AdjustedScore))
                {
                    return competitorIndex;
                }

                BasicEA.CalculateScoreAdjustment(competitor,
                        Trainer.ScoreAdjusters);
                if (!Trainer.SelectionComparer.IsBetterThan(competitor,
                        worst))
                {
                    worst = competitor;
                    worstIndex = competitorIndex;
                }
            }
            return worstIndex;
        }

        /// <inheritdoc/>
        public int PerformSelection(IGenerateRandom rnd, ISpecies species)
        {
            int bestIndex = rnd.NextInt(species.Members.Count);
            IGenome best = species.Members[bestIndex];
            BasicEA.CalculateScoreAdjustment(best, Trainer.ScoreAdjusters);

            for (int i = 0; i < Rounds; i++)
            {
                int competitorIndex = rnd.NextInt(species.Members.Count);
                IGenome competitor = species.Members[competitorIndex];

                // only evaluate valid genomes
                if (!double.IsInfinity(competitor.AdjustedScore)
                        && !double.IsNaN(competitor.AdjustedScore))
                {
                    BasicEA.CalculateScoreAdjustment(competitor,
                            Trainer.ScoreAdjusters);
                    if (Trainer.SelectionComparer.IsBetterThan(
                            competitor, best))
                    {
                        best = competitor;
                        bestIndex = competitorIndex;
                    }
                }
            }
            return bestIndex;
        }
    }
}
