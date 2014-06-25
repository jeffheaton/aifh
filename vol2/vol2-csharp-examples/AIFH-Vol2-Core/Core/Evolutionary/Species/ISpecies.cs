using AIFH_Vol2.Core.Evolutionary.Genome;
using AIFH_Vol2.Core.Evolutionary.Population;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol2.Core.Evolutionary.Species
{
    /// <summary>
    /// Define a species.
    /// </summary>
    public interface ISpecies
    {
        /// <summary>
        /// Add a genome to this species. 
        /// </summary>
        /// <param name="genome">The genome to add.</param>
        void Add(IGenome genome);

        /// <summary>
        /// Calculate this genome's share of the next population. 
        /// </summary>
        /// <param name="shouldMinimize">True if we see to minimize the score.</param>
        /// <param name="maxScore">The best score.</param>
        /// <returns>The share of this species, as a percent ratio.</returns>
        double CalculateShare(bool shouldMinimize, double maxScore);

        /// <summary>
        /// The age of this species.
        /// </summary>
        int Age { get; set; }

        /// <summary>
        /// The best score for this species.
        /// </summary>
        double BestScore { get; set; }

        /// <summary>
        /// The number of generations with no imrpvement.
        /// </summary>
        int GensNoImprovement { get; set; }

        /// <summary>
        /// The leader of this species.
        /// </summary>
        IGenome Leader { get; set; }

        /// <summary>
        /// The members of this species.
        /// </summary>
        List<IGenome> Members { get; set; }

        /// <summary>
        /// Get the offspring count.
        /// </summary>
        int OffspringCount { get; set; }

        /// <summary>
        /// The offspring share for the next iteration's population.
        /// </summary>
        double OffspringShare { get; set; }

        /// <summary>
        /// The population.
        /// </summary>
        IPopulation Population { get; set; }
    }
}
