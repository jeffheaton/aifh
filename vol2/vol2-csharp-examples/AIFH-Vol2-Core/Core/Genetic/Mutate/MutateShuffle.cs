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
using AIFH_Vol2.Core.Evolutionary.Opp;
using AIFH_Vol2.Core.Evolutionary.Train;
using AIFH_Vol2.Core.Genetic.Genome;
using AIFH_Vol2.Core.Randomize;

namespace AIFH_Vol2.Core.Genetic.Mutate
{
    /// <summary>
    ///     A simple mutation where genes are shuffled.
    ///     This mutation will not produce repeated genes.
    /// </summary>
    public class MutateShuffle : IEvolutionaryOperator
    {
        /// <summary>
        ///     The owner.
        /// </summary>
        private IEvolutionaryAlgorithm owner;

        /// <inheritdoc />
        public void PerformOperation(IGenerateRandom rnd, IGenome[] parents, int parentIndex,
            IGenome[] offspring, int offspringIndex)
        {
            var parent = (IArrayGenome) parents[parentIndex];
            offspring[offspringIndex] = owner.Population.GenomeFactory.Factor();
            var child = (IArrayGenome) offspring[offspringIndex];

            child.Copy(parent);

            int length = parent.Count;
            var iswap1 = (int) (rnd.NextDouble()*length);
            var iswap2 = (int) (rnd.NextDouble()*length);

            // can't be equal
            if (iswap1 == iswap2)
            {
                // move to the next, but
                // don't go out of bounds
                if (iswap1 > 0)
                {
                    iswap1--;
                }
                else
                {
                    iswap1++;
                }
            }

            // make sure they are in the right order
            if (iswap1 > iswap2)
            {
                int temp = iswap1;
                iswap1 = iswap2;
                iswap2 = temp;
            }

            child.Swap(iswap1, iswap2);
        }

        /// <summary>
        ///     The number of offspring produced, which is 1 for this mutation.
        /// </summary>
        public int OffspringProduced
        {
            get { return 1; }
        }

        /// <inheritdoc />
        public int ParentsNeeded
        {
            get { return 1; }
        }

        /// <inheritdoc />
        public void Init(IEvolutionaryAlgorithm theOwner)
        {
            owner = theOwner;
        }
    }
}
