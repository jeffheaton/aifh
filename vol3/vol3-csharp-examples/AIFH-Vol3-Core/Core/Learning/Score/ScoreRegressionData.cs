
using System.Collections.Generic;
using AIFH_Vol3.Core.Error;
using AIFH_Vol3.Core.General.Data;

namespace AIFH_Vol3.Core.Learning.Score
{
    /// <summary>
    /// Score regression data.  The score is done using an error calculation method.
    /// </summary>
    public class ScoreRegressionData : IScoreFunction
    {
        /// <summary>
        /// The error calculator.
        /// </summary>
        private IErrorCalculation _errorCalc = new ErrorCalculationMSE();

        /// <summary>
        /// The training data.
        /// </summary>
        private readonly IList<BasicData> _trainingData;

        /// <summary>
        /// Construct the function.
        /// </summary>
        /// <param name="theTrainingData">The training data.</param>
        public ScoreRegressionData(IList<BasicData> theTrainingData)
        {
            _trainingData = theTrainingData;
        }

        /// <inheritdoc/>
        public double CalculateScore(IMLMethod algo)
        {
            var ralgo = (IRegressionAlgorithm)algo;
            // evaulate
            _errorCalc.Clear();
            foreach (var pair in _trainingData)
            {
                double[] output = ralgo.ComputeRegression(pair.Input);
                _errorCalc.UpdateError(output, pair.Ideal, 1.0);
            }

            return _errorCalc.Calculate();
        }

        /// <summary>
        /// The error calculation method.
        /// </summary>
        public IErrorCalculation ErrorCalc
        {
            get
            {
                return _errorCalc;
            }
            set
            {
                _errorCalc = value;
            }
        }


        /// <summary>
        /// The training data.
        /// </summary>
        public IList<BasicData> TrainingData
        {
            get
            {
                return _trainingData;
            }
        }
    }
}
