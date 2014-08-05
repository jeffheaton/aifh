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

namespace AIFH_Vol2.Core.Genetic.Crossover
{
    /// <summary>
    ///     A simple cross over where genes are simply "spliced". Genes are allowed to
    ///     repeat.
    /// </summary>
    public class Splice : IEvolutionaryOperator
    {
        /// <summary>
        ///     The cut length.
        /// </summary>
        private readonly int _cutLength;

        /// <summary>
        ///     The owner.
        /// </summary>
        private IEvolutionaryAlgorithm _owner;

        /// <summary>
        ///     Create a slice crossover with the specified cut length.
        /// </summary>
        /// <param name="theCutLength">The cut length.</param>
        public Splice(int theCutLength)
        {
            _cutLength = theCutLength;
        }

        /// <inheritdoc />
        public void PerformOperation(IGenerateRandom rnd, IGenome[] parents, int parentIndex,
            IGenome[] offspring, int offspringIndex)
        {
            var mother = (IArrayGenome) parents[parentIndex];
            var father = (IArrayGenome) parents[parentIndex + 1];
            var offspring1 = (IArrayGenome) _owner.Population.GenomeFactory.Factor();
            var offspring2 = (IArrayGenome) _owner.Population.GenomeFactory.Factor();

            offspring[offspringIndex] = offspring1;
            offspring[offspringIndex + 1] = offspring2;

            int geneLength = mother.Count;

            // the chromosome must be cut at two positions, determine them
            int cutpoint1 = rnd.NextInt(geneLength - _cutLength);
            int cutpoint2 = cutpoint1 + _cutLength;

            // handle cut section
            for (int i = 0; i < geneLength; i++)
            {
                if (!((i < cutpoint1) || (i > cutpoint2)))
                {
                    offspring1.Copy(father, i, i);
                    offspring2.Copy(mother, i, i);
                }
            }

            // handle outer sections
            for (int i = 0; i < geneLength; i++)
            {
                if ((i < cutpoint1) || (i > cutpoint2))
                {
                    offspring1.Copy(mother, i, i);
                    offspring2.Copy(father, i, i);
                }
            }
        }

        /// <inheritdoc />
        public int OffspringProduced
        {
            get { return 2; }
        }

        /// <inheritdoc />
        public int ParentsNeeded
        {
            get { return 2; }
        }

        /// <inheritdoc />
        public void Init(IEvolutionaryAlgorithm theOwner)
        {
            _owner = theOwner;
        }
    }
}
