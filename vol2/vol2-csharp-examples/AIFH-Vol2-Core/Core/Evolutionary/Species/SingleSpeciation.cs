using AIFH_Vol2.Core.Evolutionary.Genome;
using AIFH_Vol2.Core.Evolutionary.Sort;
using AIFH_Vol2.Core.Evolutionary.Train;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol2.Core.Evolutionary.Species
{
    /// <summary>
    ///     This speciation strategy simply creates a single species that contains the
    ///     entire population. Use this speciation strategy if you do not wish to use
    ///     species.
    /// </summary>
    [Serializable]
    public class SingleSpeciation : ISpeciation
    {
        /// <summary>
        ///     The trainer.
        /// </summary>
        private IEvolutionaryAlgorithm _owner;

        /// <summary>
        ///     The method used to sort the genomes in the species. More desirable
        ///     genomes should come first for later selection.
        /// </summary>
        private SortGenomesForSpecies _sortGenomes;

        /// <inheritdoc />
        public void Init(IEvolutionaryAlgorithm theOwner)
        {
            _owner = theOwner;
            _sortGenomes = new SortGenomesForSpecies(_owner);
        }

        /// <inheritdoc />
        public void PerformSpeciation(IList<IGenome> genomeList)
        {
            UpdateShare();
            ISpecies species = _owner.Population.Species[0];
            species.Members.Clear();
            species.Members = species.Members.Union(genomeList).ToList();
            species.Members.Sort(_sortGenomes);
            species.Leader = species.Members[0];
        }

        /// <inheritdoc />
        private void UpdateShare()
        {
            int speciesCount = _owner.Population.Species.Count;
            if (speciesCount != 1)
            {
                throw new AIFHError(
                    "SingleSpeciation can only be used with a species count of 1, species count is "
                    + speciesCount);
            }

            ISpecies species = _owner.Population.Species[0];
            species.OffspringCount = _owner.Population.PopulationSize;
        }
    }
}
