using AIFH_Vol2.Core.Evolutionary.Genome;
using AIFH_Vol2.Core.Learning;

namespace AIFH_Vol2.Core.Genetic.Trees
{
    /// <summary>
    ///     A tree genome, used for Genetic Programming.
    /// </summary>
    public class TreeGenome : BasicGenome, IRegressionAlgorithm
    {
        private readonly EvaluateTree _evaluator;

        /// <summary>
        ///     Constructor.
        /// </summary>
        /// <param name="theEvaluator">The evaluator.</param>
        public TreeGenome(EvaluateTree theEvaluator)
        {
            _evaluator = theEvaluator;
        }

        public TreeGenomeNode Root { get; set; }

        /// <inheritdoc />
        public override int Count
        {
            get { return Root.Count; }
        }

        public EvaluateTree Evaluator
        {
            get { return _evaluator; }
        }

        /// <inheritdoc />
        public override double[] LongTermMemory
        {
            get { throw new AIFHError("Long term memory not supported, use a genetic trainer."); }
        }

        /// <inheritdoc />
        public double[] ComputeRegression(double[] input)
        {
            var result = new double[1];
            result[0] = _evaluator.Evaluate(Root, input);
            return result;
        }

        /// <inheritdoc />
        public override void Copy(IGenome source)
        {
            Root = ((TreeGenome) source).Root.Copy();
        }
    }
}