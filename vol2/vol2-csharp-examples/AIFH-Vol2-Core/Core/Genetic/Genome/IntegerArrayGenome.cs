// Artificial Intelligence for Humans
// Volume 2: Nature-Inspired Algorithms
// C# Version
// http://www.aifh.org
// http://www.jeffheaton.com
//
// Code repository:
// https://github.com/jeffheaton/aifh
//
// Copyright 2014 by Jeff Heaton
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// For more information on Heaton Research copyrights, licenses
// and trademarks visit:
// http://www.heatonresearch.com/copyright
//
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
