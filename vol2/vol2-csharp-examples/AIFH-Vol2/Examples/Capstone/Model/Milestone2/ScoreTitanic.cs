using System.Collections.Generic;
using AIFH_Vol2.Core.General.Data;
using AIFH_Vol2.Core.Learning;
using AIFH_Vol2.Core.Learning.Score;

namespace AIFH_Vol2.Examples.Capstone.Model.Milestone2
{
    /// <summary>
    ///     Score the Titanic model. The score is percentage cases predicted correctly.
    /// </summary>
    public class ScoreTitanic : IScoreFunction
    {
        /// <summary>
        ///     The training data.
        /// </summary>
        private readonly IList<BasicData> _trainingData;

        /// <summary>
        ///     Construct the score function.
        /// </summary>
        /// <param name="theTrainingData">The training data.</param>
        public ScoreTitanic(IList<BasicData> theTrainingData)
        {
            _trainingData = theTrainingData;
        }

        /// <inheritdoc />
        public double CalculateScore(IMLMethod algo)
        {
            int incorrectCount = 0;
            int totalCount = 0;

            var alg = (IRegressionAlgorithm) algo;

            foreach (BasicData aTrainingData in _trainingData)
            {
                totalCount++;
                bool predictSurvive = alg.ComputeRegression(aTrainingData.Input)[0] > 0.5;
                bool idealSurvive = aTrainingData.Ideal[0] > 0.5;

                if (predictSurvive == idealSurvive)
                {
                    incorrectCount++;
                }
            }

            return incorrectCount/(double) totalCount;
        }

        /// <inheritdoc />
        public bool ShouldMinimize
        {
            get { return false; }
        }
    }
}