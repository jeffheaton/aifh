using AIFH_Vol2.Core.Evolutionary.Genome;
using System;

namespace AIFH_Vol2.Core.Evolutionary.Sort
{
    /// <summary>
    ///     Use this comparator to minimize the adjusted score.
    /// </summary>
    [Serializable]
    public class MinimizeAdjustedScoreComp : AbstractGenomeComparer
    {
        /// <inheritdoc />
        public override bool ShouldMinimize
        {
            get { return true; }
        }

        /// <inheritdoc />
        public override int Compare(IGenome p1, IGenome p2)
        {
            return p1.AdjustedScore.CompareTo(p2.AdjustedScore);
        }

        /// <inheritdoc />
        public override bool IsBetterThan(IGenome prg, IGenome betterThan)
        {
            return IsBetterThan(prg.AdjustedScore,
                                betterThan.AdjustedScore);
        }
    }
}
