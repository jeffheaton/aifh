using AIFH_Vol2.Core.Evolutionary.Population;
using AIFH_Vol2.Core.Evolutionary.Species;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol2.Core.Evolutionary.Genome
{
    /// <summary>
    /// A basic abstract genome. Provides base functionality.
    /// </summary>
    [Serializable]
    public abstract class BasicGenome : IGenome
    {
        /// <summary>
        /// The adjusted score. If unknown, it is set to NaN.
        /// </summary>
        public double AdjustedScore { get; set; }

        /// <summary>
        /// The score of this genome.
        /// </summary>
        public double Score { get; set; }

        /// <summary>
        /// The population this genome belongs to.
        /// </summary>
        public IPopulation Population { get; set; }

        /// <summary>
        /// The birth generation for this genome.
        /// </summary>
        public int BirthGeneration { get; set; }

        /// <summary>
        /// The species of this genome.
        /// </summary>
        public ISpecies Species { get; set; }

        /// <summary>
        /// Construct a basic genome.
        /// </summary>
        public BasicGenome()
        {
            Score = double.NaN;
            AdjustedScore = double.NaN;
        }

        /// <inheritdoc/>
        public String ToString()
        {
            StringBuilder builder = new StringBuilder();
            builder.Append("[");
            builder.Append(this.GetType().Name);
            builder.Append(": score=");
            builder.Append(Score);
            return builder.ToString();
        }


        /// <inheritdoc/>
        public abstract void Copy(IGenome source);

        /// <inheritdoc/>
        public abstract int Count { get; }

        /// <inheritdoc/>
        public abstract double[] LongTermMemory { get; }
    }
}
