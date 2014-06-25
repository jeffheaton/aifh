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
