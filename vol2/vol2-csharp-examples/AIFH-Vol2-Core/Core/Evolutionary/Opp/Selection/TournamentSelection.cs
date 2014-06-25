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