using System.Collections.Generic;
using AIFH_Vol2.Core.Evolutionary.Genome;
using AIFH_Vol2.Core.Evolutionary.Opp;
using AIFH_Vol2.Core.Evolutionary.Train;
using AIFH_Vol2.Core.Genetic.Genome;
using AIFH_Vol2.Core.Randomize;

namespace AIFH_Vol2.Core.Genetic.Crossover
{
    /// <summary>
    ///     A simple cross over where genes are simply "spliced". Genes are not allowed
    ///     to repeat.  This method only works with IntegerArrayGenome.
    /// </summary>
    public class SpliceNoRepeat : IEvolutionaryOperator
    {
        /// <summary>
        ///     The cut length.
        /// </summary>
        private readonly int _cutLength;

        /// <summary>
        ///     The owner.
        /// </summary>
        private IEvolutionaryAlgorithm _owner;

        /**
         * Construct a splice crossover.
         * 
         * @param theCutLength
         *            The cut length.
         */

        public SpliceNoRepeat(int theCutLength)
        {
            _cutLength = theCutLength;
        }

        /// <inheritdoc />
        public void PerformOperation(IGenerateRandom rnd, IGenome[] parents, int parentIndex,
            IGenome[] offspring, int offspringIndex)
        {
            var mother = (IntegerArrayGenome) parents[parentIndex];
            var father = (IntegerArrayGenome) parents[parentIndex + 1];
            var offspring1 = (IntegerArrayGenome) _owner.Population.GenomeFactory.Factor();
            var offspring2 = (IntegerArrayGenome) _owner.Population.GenomeFactory.Factor();

            offspring[offspringIndex] = offspring1;
            offspring[offspringIndex + 1] = offspring2;

            int geneLength = mother.Count;

            // the chromosome must be cut at two positions, determine them
            int cutpoint1 = rnd.NextInt(geneLength - _cutLength);
            int cutpoint2 = cutpoint1 + _cutLength;

            // keep track of which genes have been taken in each of the two
            // offspring, defaults to false.
            var taken1 = new HashSet<int>();
            var taken2 = new HashSet<int>();

            // handle cut section
            for (int i = 0; i < geneLength; i++)
            {
                if (!((i < cutpoint1) || (i > cutpoint2)))
                {
                    offspring1.Copy(father, i, i);
                    offspring2.Copy(mother, i, i);
                    taken1.Add(father.Data[i]);
                    taken2.Add(mother.Data[i]);
                }
            }

            // handle outer sections
            for (int i = 0; i < geneLength; i++)
            {
                if ((i < cutpoint1) || (i > cutpoint2))
                {
                    offspring1.Data[i] = GetNotTaken(mother, taken1);
                    offspring2.Data[i] = GetNotTaken(father, taken2);
                }
            }
        }

        /// <summary>
        ///     The number of offspring produced, which is 2 for splice crossover.
        /// </summary>
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

        /// <summary>
        ///     Get a list of the genes that have not been taken before. This is useful
        ///     if you do not wish the same gene to appear more than once in a
        ///     genome.
        /// </summary>
        /// <param name="source">The pool of genes to select from.</param>
        /// <param name="taken"> An array of the taken genes.</param>
        /// <returns>Those genes in source that are not taken.</returns>
        private static int GetNotTaken(IntegerArrayGenome source,
            HashSet<int> taken)
        {
            foreach (int trial in source.Data)
            {
                if (!taken.Contains(trial))
                {
                    taken.Add(trial);
                    return trial;
                }
            }

            throw new AIFHError("Ran out of integers to select.");
        }
    }
}