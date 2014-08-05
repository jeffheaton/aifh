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
using AIFH_Vol2.Core.General.Collections;
using AIFH_Vol2.Core.Randomize;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol2.Core.Evolutionary.Opp
{
    /// <summary>
    /// A compound operator randomly chooses sub-operators to perform the actual
    /// operation. Each of the sub-operators can be provided with a weighting.
    /// </summary>
    public class CompoundOperator : IEvolutionaryOperator
    {
        /// <summary>
        /// The owner of this operator.
        /// </summary>
        private IEvolutionaryAlgorithm _owner;

        /// <summary>
        /// The sub-operators that make up this compound operator.
        /// </summary>
        private OperationList _components = new OperationList();

        /// <summary>
        /// The components.
        /// </summary>
        public OperationList Components
        {
            get
            {
                return _components;
            }
        }

        /// <summary>
        /// The owner.
        /// </summary>
        public IEvolutionaryAlgorithm Owner
        {
            get
            {
                return _owner;
            }
        }

        /// <inheritdoc/>
        public void Init(IEvolutionaryAlgorithm theOwner)
        {
            _owner = theOwner;
            foreach (ObjectHolder<IEvolutionaryOperator> obj in _components
                    .ObjList)
            {
                obj.Obj.Init(theOwner);
            }
        }

        /// <inheritdoc/>
        public int OffspringProduced
        {
            get
            {
                return _components.MaxOffspring;
            }
        }

        /// <inheritdoc/>
        public int ParentsNeeded
        {
            get
            {
                return _components.MaxOffspring;
            }
        }

        /// <inheritdoc/>
        public void PerformOperation(IGenerateRandom rnd, IGenome[] parents,
                                     int parentIndex, IGenome[] offspring,
                                     int offspringIndex)
        {
            IEvolutionaryOperator opp = _components.Pick(rnd);
            opp.PerformOperation(rnd, parents, parentIndex, offspring,
                    offspringIndex);
        }
    }
}
