using AIFH_Vol2.Core.Evolutionary.Genome;
using System;

namespace AIFH_Vol2.Core.Evolutionary.Sort
{
    /// <summary>
    ///     Provides base functionality for comparing genomes. Specifically the ability
    ///     to add bonuses and penalties.
    /// </summary>
    [Serializable]
    public abstract class AbstractGenomeComparer : IGenomeComparer
    {
        /// <inheritdoc />
        public double ApplyBonus(double value, double bonus)
        {
            double amount = value*bonus;
            if (ShouldMinimize)
            {
                return value - amount;
            }
            return value + amount;
        }

        /// <inheritdoc />
        public double ApplyPenalty(double value, double bonus)
        {
            double amount = value*bonus;
            if (!ShouldMinimize)
            {
                return value - amount;
            }
            return value + amount;
        }

        /// <inheritdoc />
        public bool IsBetterThan(double d1, double d2)
        {
            if (ShouldMinimize)
            {
                return d1 < d2;
            }
            return d1 > d2;
        }


        public abstract bool IsBetterThan(IGenome genome1, IGenome genome2);

        public abstract bool ShouldMinimize { get; }

        /// <summary>
        ///     Compare two genomes.
        /// </summary>
        /// <param name="x">The first genome.</param>
        /// <param name="y">The second genome.</param>
        /// <returns>
        ///     0 if equal, &lt;0 if x is less,&gt;0 if y is less.
        /// </returns>
        public abstract int Compare(IGenome x, IGenome y);
    }
}
