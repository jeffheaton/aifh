using System;
using AIFH_Vol2.Core.Evolutionary.Genome;
using AIFH_Vol2.Core.Evolutionary.Opp;
using AIFH_Vol2.Core.Evolutionary.Train;
using AIFH_Vol2.Core.Randomize;

namespace AIFH_Vol2.Core.Genetic.Trees
{
    /// <summary>
    ///     Create a child tree as a mutation of the parent.  Do not modify the parent.
    /// </summary>
    public class MutateTree : IEvolutionaryOperator
    {
        /// <summary>
        ///     The maximum length of a branch to graft.
        /// </summary>
        private readonly int _maxGraftLength;

        /// <summary>
        ///     The owner.
        /// </summary>
        private IEvolutionaryAlgorithm _owner;

        /// <summary>
        ///     Construct the tree mutation object.
        /// </summary>
        /// <param name="theMaxGraftLength">The maximum graft length.</param>
        public MutateTree(int theMaxGraftLength)
        {
            _maxGraftLength = Math.Max(1, theMaxGraftLength);
        }

        /// <inheritdoc />
        public void Init(IEvolutionaryAlgorithm theOwner)
        {
            _owner = theOwner;
        }

        /// <inheritdoc />
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
        public void PerformOperation(IGenerateRandom rnd, IGenome[] parents, int parentIndex, IGenome[] offspring,
            int offspringIndex)
        {
            var parent1 = (TreeGenome) parents[parentIndex];
            EvaluateTree eval = parent1.Evaluator;
            var off1 = (TreeGenome) _owner.Population.GenomeFactory.Factor(parent1);
            RandomNodeResult off1Point = eval.SampleRandomNode(rnd, off1.Root);

            int len = rnd.NextInt(1, _maxGraftLength + 1);
            TreeGenomeNode randomSequence = eval.Grow(rnd, len);

            if (off1Point.Parent == null)
            {
                off1.Root = randomSequence;
            }
            else
            {
                int idx = off1Point.Parent.Children.IndexOf(off1Point.Child);
                off1Point.Parent.Children[idx] = randomSequence;
            }

            offspring[0] = off1;
        }
    }
}