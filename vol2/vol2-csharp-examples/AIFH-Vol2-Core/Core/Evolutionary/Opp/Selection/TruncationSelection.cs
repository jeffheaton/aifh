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
