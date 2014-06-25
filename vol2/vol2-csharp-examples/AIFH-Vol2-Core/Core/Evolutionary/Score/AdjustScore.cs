using AIFH_Vol2.Core.Evolutionary.Genome;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol2.Core.Evolutionary.Score
{
    /// <summary>
    ///     Score adjusters adjust the score according to some means. The resulting score
    ///     is stored in the genome's adjusted score.
    /// </summary>
    public interface IAdjustScore
    {
        /// <summary>
        /// Calculate the score adjustment.
        /// </summary>
        /// <param name="genome">The genome.</param>
        /// <returns>The adjusted score.</returns>
        double CalculateAdjustment(IGenome genome);
    }
}
