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
using AIFH_Vol2.Core.Randomize;

namespace AIFH_Vol2.Core.Genetic.Trees
{
    /// <summary>
    ///     Cross over using a tree. Used for genetic programming.
    /// </summary>
    public class CrossoverTree : IEvolutionaryOperator
    {
        private IEvolutionaryAlgorithm _owner;

        /// <inheritdoc/>
        public void Init(IEvolutionaryAlgorithm theOwner)
        {
            _owner = theOwner;
        }

        /// <inheritdoc/>
        public int OffspringProduced
        {
            get { return 1; }
        }

        /// <inheritdoc/>
        public int ParentsNeeded
        {
            get { return 1; }
        }

        /// <inheritdoc/>
        public void PerformOperation(IGenerateRandom rnd, IGenome[] parents, int parentIndex, IGenome[] offspring,
            int offspringIndex)
        {
            var parent1 = (TreeGenome) parents[parentIndex];
            var parent2 = (TreeGenome) parents[parentIndex];
            EvaluateTree eval = parent1.Evaluator;

            var off1 = (TreeGenome) _owner.Population.GenomeFactory.Factor(parent1);
            var replacePoint = eval.SampleRandomNode(rnd, off1.Root);
            var copySource = eval.SampleRandomNode(rnd, parent2.Root);
            var actualCopy = copySource.Child.Copy();

            if (replacePoint.Parent == null)
            {
                off1.Root = actualCopy;
            }
            else
            {
                var idx = replacePoint.Parent.Children.IndexOf(replacePoint.Child);
                replacePoint.Parent.Children[idx] = actualCopy;
            }

            offspring[0] = off1;
        }
    }
}
