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
