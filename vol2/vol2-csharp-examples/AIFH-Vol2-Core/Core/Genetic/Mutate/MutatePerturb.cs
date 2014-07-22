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