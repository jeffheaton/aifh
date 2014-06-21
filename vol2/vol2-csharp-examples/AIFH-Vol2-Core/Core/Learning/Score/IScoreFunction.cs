using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol2.Core.Learning.Score
{
    /// <summary>
    /// A score function evaluates a Machine Learning algorithm.  
    /// We will seek to either minimize or maximize the score.
    /// </summary>
    public interface IScoreFunction
    {
        /// <summary>
        /// Calculate a score for the specified algorithm.
        /// </summary>
        /// <param name="algo">The algorithm to score.</param>
        /// <returns>The score.</returns>
        double CalculateScore(IMLMethod algo);

        /// <summary>
        /// True if the goal is to minimize the score.
        /// </summary>
        bool ShouldMinimize { get; }

    }
}
