using AIFH_Vol2.Core.Evolutionary.Genome;
using System;

namespace AIFH_Vol2.Core.Evolutionary.Sort
{
    /// <summary>
    ///     Use this comparator to maximize the score.
    /// </summary>
    [Serializable]
    public class MaximizeScoreComp : AbstractGenomeComparer
    {
        /// <inheritdoc />
        public override bool ShouldMinimize
        {
            get { return false; }
        }

        /// <inheritdoc />
        public override int Compare(IGenome p1, IGenome p2)
        {
            return p2.Score.CompareTo(p1.Score);
        }

        /// <inheritdoc />
        public override bool IsBetterThan(IGenome prg, IGenome betterThan)
        {
            return IsBetterThan(prg.AdjustedScore,
                                betterThan.AdjustedScore);
        }
    }
}
