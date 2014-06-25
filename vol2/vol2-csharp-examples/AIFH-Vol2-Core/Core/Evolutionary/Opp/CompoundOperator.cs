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
