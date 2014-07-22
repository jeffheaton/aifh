using AIFH_Vol2.Core.Evolutionary.Genome;

namespace AIFH_Vol2.Core.Genetic.Trees
{
    /// <summary>
    ///     Used to factor tree genomes.
    /// </summary>
    public class TreeGenomeFactory : IGenomeFactory
    {
        /// <summary>
        ///     Evaluate the tree.
        /// </summary>
        private readonly EvaluateTree _eval;

        /// <summary>
        ///     Construct a tree factory.
        /// </summary>
        /// <param name="theEval">The evalator.</param>
        public TreeGenomeFactory(EvaluateTree theEval)
        {
            _eval = theEval;
        }

        /// <inheritdoc />
        public IGenome Factor()
        {
            return new TreeGenome(_eval);
        }

        /// <inheritdoc />
        public IGenome Factor(IGenome other)
        {
            var result = new TreeGenome(_eval);
            result.Copy(other);
            return result;
        }
    }
}