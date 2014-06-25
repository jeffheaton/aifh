using AIFH_Vol2.Core.Evolutionary.Species;
using AIFH_Vol2.Core.Evolutionary.Train;
using System.Collections.Generic;

namespace AIFH_Vol2.Core.Evolutionary.Sort
{
    /// <summary>
    ///     This comparator is used to compare two species. This is done by comparing the
    ///     scores of the two leaders.
    /// </summary>
    public class SpeciesComparer : Comparer<ISpecies>
    {
        /// <summary>
        ///     The training method.
        /// </summary>
        private readonly IEvolutionaryAlgorithm _training;

        /// <summary>
        ///     Create a species comparator.
        /// </summary>
        /// <param name="theTraining">The trainer.</param>
        public SpeciesComparer(IEvolutionaryAlgorithm theTraining)
        {
            _training = theTraining;
        }

        /// <inheritdoc />
        public override int Compare(ISpecies sp1, ISpecies sp2)
        {
            return _training.BestComparer.Compare(sp1.Leader,
                                                 sp2.Leader);
        }
    }
}
