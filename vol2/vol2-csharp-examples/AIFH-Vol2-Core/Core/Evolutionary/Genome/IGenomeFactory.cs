using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol2.Core.Evolutionary.Genome
{
    /// <summary>
    /// Defines a factory that produces genomes.
    /// </summary>
    public interface IGenomeFactory
    {
        /// <summary>
        /// The newly created genome.
        /// </summary>
        /// <returns></returns>
        IGenome Factor();

        /// <summary>
        /// Create a clone of the other genome. 
        /// </summary>
        /// <param name="other">The other genome.</param>
        /// <returns>The newly created clone.</returns>
        IGenome Factor(IGenome other);
    }
}
