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
    /// This class holds a list of evolutionary operators. Each operator is given a
    /// probability weight. Based on the number of parents available a random
    /// selection of an operator can be made based on the probability given each of
    /// the operators.
    /// </summary>
    [Serializable]
    public class OperationList : ChooseObject<IEvolutionaryOperator>
    {
        /// <summary>
        /// Determine the maximum number of offspring that might be produced by any
        /// of the operators in this list.
        /// </summary>
        public int MaxOffspring
        {
            get
            {
                int result = 0;
                foreach (ObjectHolder<IEvolutionaryOperator> holder in ObjList)
                {
                    result = Math.Max(result, holder.Obj.OffspringProduced);
                }
                return result;
            }
        }

        /// <summary>
        /// Determine the maximum number of parents required by any of the operators
        /// in the list.
        /// </summary>
        public int MaxParents
        {
            get
            {
                int result = int.MinValue;
                foreach (ObjectHolder<IEvolutionaryOperator> holder in ObjList)
                {
                    result = Math.Max(result, holder.Obj.ParentsNeeded);
                }
                return result;
            }
        }

        /// <summary>
        /// Pick a operator based on the number of parents available.
        /// </summary>
        /// <param name="rnd">A random number generator.</param>
        /// <param name="maxParents">The maximum number of parents available.</param>
        /// <returns>The operator that was selected.</returns>
        public IEvolutionaryOperator PickMaxParents(IGenerateRandom rnd,
                                                   int maxParents)
        {

            // determine the total probability of eligible operators
            double total = 0;
            foreach (ObjectHolder<IEvolutionaryOperator> holder in ObjList)
            {
                if (holder.Obj.ParentsNeeded <= maxParents)
                {
                    total += holder.Probability;
                }
            }

            // choose an operator
            double r = rnd.NextDouble() * total;
            double current = 0;
            foreach (ObjectHolder<IEvolutionaryOperator> holder in ObjList)
            {
                if (holder.Obj.ParentsNeeded <= maxParents)
                {
                    current += holder.Probability;
                    if (r < current)
                    {
                        return holder.Obj;
                    }
                }
            }

            return null;
        }

    }
}
