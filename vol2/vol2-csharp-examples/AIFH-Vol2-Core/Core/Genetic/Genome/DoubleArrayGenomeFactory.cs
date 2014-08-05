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
using AIFH_Vol2.Core.Evolutionary.Genome;

namespace AIFH_Vol2.Core.Genetic.Genome
{
    /// <summary>
    ///     A factory that creates DoubleArrayGenome objects of a specific size.
    /// </summary>
    public class DoubleArrayGenomeFactory : IGenomeFactory
    {
        /// <summary>
        ///     The size to create.
        /// </summary>
        private readonly int _size;

        /// <summary>
        ///     Construct the genome factory.
        /// </summary>
        /// <param name="theSize">The size to create genomes of.</param>
        public DoubleArrayGenomeFactory(int theSize)
        {
            _size = theSize;
        }

        /// <inheritdoc />
        public IGenome Factor()
        {
            return new DoubleArrayGenome(_size);
        }

        /// <inheritdoc />
        public IGenome Factor(IGenome other)
        {
            // TODO Auto-generated method stub
            return new DoubleArrayGenome((DoubleArrayGenome) other);
        }
    }
}
