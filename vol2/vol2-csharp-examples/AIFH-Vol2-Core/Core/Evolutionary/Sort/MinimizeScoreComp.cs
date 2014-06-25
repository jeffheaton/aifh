using AIFH_Vol2.Core.Evolutionary.Genome;
using System;

namespace AIFH_Vol2.Core.Evolutionary.Sort
{
    /// <summary>
    ///     Use this comparator to minimize the score.
    /// </summary>
    [Serializable]
    public class MinimizeScoreComp : AbstractGenomeComparer
    {
        /// <inheritdoc />
        public override bool ShouldMinimize
        {
            get { return true; }
        }

        /// <inheritdoc />
        public override int Compare(IGenome p1, IGenome p2)
        {
            return p1.Score.CompareTo(p2.Score);
        }

        /// <inheritdoc />
        public override bool IsBetterThan(IGenome prg, IGenome betterThan)
        {
            return IsBetterThan(prg.AdjustedScore,
                                betterThan.AdjustedScore);
        }
    }
}
