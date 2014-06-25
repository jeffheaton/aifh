using AIFH_Vol2.Core.Evolutionary.Genome;
using AIFH_Vol2.Core.Evolutionary.Train;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol2.Core.Evolutionary.Species
{
    /// <summary>
    /// Defines a speciation strategy.
    /// </summary>
    public interface ISpeciation
    {
        /// <summary>
        /// Setup the speciation strategy.
        /// </summary>
        /// <param name="theOwner">The owner.</param>
        void Init(IEvolutionaryAlgorithm theOwner);

        /// <summary>
        /// Perform the speciation.
        /// </summary>
        /// <param name="genomeList">The genomes to speciate.</param>
        void PerformSpeciation(IList<IGenome> genomeList);
    }
}
