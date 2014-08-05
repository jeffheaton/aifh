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
    ///     A simple mutation based on random numbers.
    /// </summary>
    public class MutatePerturb : IEvolutionaryOperator
    {
        /// <summary>
        ///     The amount to perturb by.
        /// </summary>
        private readonly double perturbAmount;

        /// <summary>
        ///     Construct a perturb mutation.
        /// </summary>
        /// <param name="thePerturbAmount">The amount to mutate by(percent).</param>
        public MutatePerturb(double thePerturbAmount)
        {
            perturbAmount = thePerturbAmount;
        }

        /// <inheritdoc />
        public void PerformOperation(IGenerateRandom rnd, IGenome[] parents, int parentIndex,
            IGenome[] offspring, int offspringIndex)
        {
            var parent = (DoubleArrayGenome) parents[parentIndex];
            offspring[offspringIndex] = parent.Population.GenomeFactory.Factor();
            var child = (DoubleArrayGenome) offspring[offspringIndex];

            for (int i = 0; i < parent.Count; i++)
            {
                double value = parent.Data[i];
                value += (perturbAmount - (rnd.NextDouble()*perturbAmount*2));
                child.Data[i] = value;
            }
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
            // not needed
        }
    }
}
