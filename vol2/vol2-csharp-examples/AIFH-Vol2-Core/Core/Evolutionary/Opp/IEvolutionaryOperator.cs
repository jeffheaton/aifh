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
using AIFH_Vol2.Core.Evolutionary.Train;
using AIFH_Vol2.Core.Randomize;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol2.Core.Evolutionary.Opp
{
    /// <summary>
    /// An evolutionary operator is used to create new offspring genomes based on
    /// parent genomes. There are a variety of means by which this can be done. The
    /// number of parents required, as well as the number of offspring produced are
    /// dependent on the operator. This interface defines key characteristics that
    /// all operators must share.
    ///
    /// Most operators do not modify the parents. However, some mutation operators do
    /// require that the children and parent array be the same. If the children and
    /// parent arrays are the same, then the parent will be mutated.
    /// </summary>
    public interface IEvolutionaryOperator
    {
        /// <summary>
        /// Called to setup the evolutionary operator.
        /// </summary>
        /// <param name="theOwner">The evolutionary algorithm used with this operator.</param>
        void Init(IEvolutionaryAlgorithm theOwner);

        /// <summary>
        /// The number of offspring produced by this type of crossover.
        /// </summary>
        int OffspringProduced { get; }

        /// <summary>
        /// The number of parents needed.
        /// </summary>
        int ParentsNeeded { get; }

        /// <summary>
        /// Perform the evolutionary operation. 
        /// </summary>
        /// <param name="rnd">A random number generator.</param>
        /// <param name="parents">The parents.</param>
        /// <param name="parentIndex">The index into the parents array.</param>
        /// <param name="offspring">The offspring.</param>
        /// <param name="offspringIndex">An index into the offspring array.</param>
        void PerformOperation(IGenerateRandom rnd, IGenome[] parents, int parentIndex,
                              IGenome[] offspring, int offspringIndex);
    }
}
