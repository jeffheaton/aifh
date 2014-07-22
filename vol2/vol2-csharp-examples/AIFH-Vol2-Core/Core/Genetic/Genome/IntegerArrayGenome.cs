using System;
using AIFH_Vol2.Core.Evolutionary.Genome;

namespace AIFH_Vol2.Core.Genetic.Genome
{
    /// <summary>
    ///     A genome that is an array of discrete integer values.
    /// </summary>
    public class IntegerArrayGenome : BasicGenome, IArrayGenome
    {
        /// <summary>
        ///     The genome data.
        /// </summary>
        private readonly int[] _data;

        /// <summary>
        ///     Construct the genome.
        /// </summary>
        /// <param name="size">The size of the genome.</param>
        public IntegerArrayGenome(int size)
        {
            _data = new int[size];
        }

        /// <summary>
        ///     Construct the genome by copying another.
        /// </summary>
        /// <param name="other">The other genome.</param>
        public IntegerArrayGenome(IntegerArrayGenome other)
        {
            _data = (int[]) other.Data.Clone();
        }

        /// <summary>
        ///     The data.
        /// </summary>
        public int[] Data
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
            var sourceInt = (IntegerArrayGenome) source;
            _data[targetIndex] = sourceInt._data[sourceIndex];
        }

        /// <inheritdoc />
        public override void Copy(IGenome source)
        {
            var sourceInt = (IntegerArrayGenome) source;
            Array.Copy(sourceInt._data, _data, _data.Length);
            Score = source.Score;
            AdjustedScore = source.AdjustedScore;
        }

        /// <inheritdoc />
        public void Swap(int iswap1, int iswap2)
        {
            int temp = _data[iswap1];
            _data[iswap1] = _data[iswap2];
            _data[iswap2] = temp;
        }

        public override double[] LongTermMemory
        {
            get { throw new NotImplementedException(); }
        }
    }
}