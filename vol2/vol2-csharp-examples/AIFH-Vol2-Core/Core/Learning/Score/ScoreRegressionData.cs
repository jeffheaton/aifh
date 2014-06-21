using AIFH_Vol2.Core.Error;
using AIFH_Vol2.Core.General.Data;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol2.Core.Learning.Score
{
    /// <summary>
    /// Score regression data.  The score is done using an error calculation method.
    /// </summary>
    public class ScoreRegressionData : IScoreFunction
    {
        /// <summary>
        /// The error calculator.
        /// </summary>
        private IErrorCalculation ErrorCalc { get; set; }

        /// <summary>
        /// The training data.
        /// </summary>
        private IList<BasicData> _trainingData;

        /// <summary>
        /// Construct the function. 
        /// </summary>
        /// <param name="theTrainingData">The training data.</param>
        public ScoreRegressionData(IList<BasicData> theTrainingData)
        {
            _trainingData = theTrainingData;
            ErrorCalc = new ErrorCalculationMSE();
        }

        /// <inheritdoc/>
        public double CalculateScore(IMLMethod algo)
        {
            IErrorCalculation ec = ErrorCalc.Create();

            IRegressionAlgorithm ralgo = (IRegressionAlgorithm)algo;
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
        /// The training data.
        /// </summary>
        public IList<BasicData> TrainingData
        {
            get
            {
                return _trainingData;
            }
        }

        /// <summary>
        /// True, this scoring method seeks to minimize.
        /// </summary>
        public bool ShouldMinimize
        {
            get
            {
                return true;
            }
        }
    }
}
