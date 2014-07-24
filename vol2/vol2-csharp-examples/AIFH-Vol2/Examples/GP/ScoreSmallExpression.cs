using System.Collections.Generic;
using AIFH_Vol2.Core.Error;
using AIFH_Vol2.Core.Evolutionary.Genome;
using AIFH_Vol2.Core.General.Data;
using AIFH_Vol2.Core.Learning;
using AIFH_Vol2.Core.Learning.Score;

namespace AIFH_Vol2.Examples.GP
{
    /// <summary>
    ///     Score regression data.  The score is done using an error calculation method.
    /// </summary>
    public class ScoreSmallExpression : IScoreFunction
    {
        /// <summary>
        ///     The maximum tree size.
        /// </summary>
        private readonly int _maxLength;

        /// <summary>
        ///     The training data.
        /// </summary>
        private readonly IList<BasicData> _trainingData;

        /// <summary>
        ///     Construct the function.
        /// </summary>
        /// <param name="theTrainingData">The training data.</param>
        /// <param name="theMaxLength">The maximum tree size.</param>
        public ScoreSmallExpression(IList<BasicData> theTrainingData, int theMaxLength)
        {
            _trainingData = theTrainingData;
            _maxLength = theMaxLength;
            ErrorCalc = new ErrorCalculationMSE();
        }

        /// <summary>
        ///     The error calculator.
        /// </summary>
        public IErrorCalculation ErrorCalc { get; set; }

        /// <inheritdoc />
        public double CalculateScore(IMLMethod algo)
        {
            IErrorCalculation ec = ErrorCalc.Create();

            var ralgo = (IRegressionAlgorithm) algo;
            var genome = (IGenome) ralgo;

            if (genome.Count > _maxLength)
            {
                return double.PositiveInfinity;
            }

            // evaulate
            ec.Clear();
            foreach (BasicData pair in _trainingData)
            {
                double[] output = ralgo.ComputeRegression(pair.Input);
                ec.UpdateError(output, pair.Ideal, 1.0);
            }

            return ec.Calculate();
        }

        /// <summary>
        ///     True, this scoring method seeks to minimize.
        /// </summary>
        public bool ShouldMinimize
        {
            get { return true; }
        }

        /// <summary>
        ///     The training data.
        /// </summary>
        /// <returns>The training data.</returns>
        public IList<BasicData> TrainingData()
        {
            return _trainingData;
        }
    }
}