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
