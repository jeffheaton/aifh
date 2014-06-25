using AIFH_Vol2.Core.Evolutionary.Genome;
using AIFH_Vol2.Core.Learning;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol2.Core.Evolutionary.CODEC
{
    /// <summary>
    /// A CODEC defines how to transfer between a genome and phenome. Every CODEC
 /// should support genome to phenome. However, not every code can transform a
/// phenome into a genome.
    /// </summary>
    public interface IGeneticCODEC
    {
        /// <summary>
        /// Decode the specified genome into a phenome. A phenome is an actual
        /// instance of a genome that you can query.
        /// </summary>
        /// <param name="genome">The genome to decode.</param>
        /// <returns>The phenome.</returns>
        IMLMethod Decode(IGenome genome);

        /// <summary>
        /// Attempt to build a genome from a phenome. Note: not all CODEC's support
        /// this. If it is unsupported, an exception will be thrown. 
        /// </summary>
        /// <param name="phenotype">The phenotype.</param>
        /// <returns>The genome.</returns>
        IGenome Encode(IMLMethod phenotype);
    }
}
