using System;
using AIFH_Vol2.Core.Evolutionary.Genome;

namespace AIFH_Vol2.Core.Genetic.Genome
{
    /// <summary>
    ///     A genome made up of continuous doubles.
    /// </summary>
    public class DoubleArrayGenome : BasicGenome, IArrayGenome
    {
        /// <summary>
        ///     The data.
        /// </summary>
        private readonly double[] _data;

        /// <summary>
        ///     Construct a genome of a specific size.
        /// </summary>
        /// <param name="size">The size.</param>
        public DoubleArrayGenome(int size)
        {
            _data = new double[size];
        }

        /// <summary>
        ///     Construct a genome based on another genome.
        /// </summary>
        /// <param name="other">The other genome.</param>
        public DoubleArrayGenome(DoubleArrayGenome other)
        {
            _data = (double[]) other.Data.Clone();
        }

        /// <summary>
        ///     The data.
        /// </summary>
        public double[] Data
        {
            get { return _data; }
        }

        /// <inheritdoc />
        public override int Count
        {
            get { return _data.Length; }
        }

        /// <inheritdoc />
        public void Copy(IArrayGenome source, int sourceIndex, int targetIndex)
        {
            var sourceInt = (DoubleArrayGenome) source;
            _data[targetIndex] = sourceInt._data[sourceIndex];
        }

        /// <inheritdoc />
        public override void Copy(IGenome source)
        {
            var sourceDouble = (DoubleArrayGenome) source;
            Array.Copy(sourceDouble._data, _data, _data.Length);
            Score = source.Score;
            AdjustedScore = source.AdjustedScore;
        }

        /// <inheritdoc />
        public void Swap(int iswap1, int iswap2)
        {
            double temp = _data[iswap1];
            _data[iswap1] = _data[iswap2];
            _data[iswap2] = temp;
        }

        public override double[] LongTermMemory
        {
            get { return _data; }
        }
    }
}