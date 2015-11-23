

using System.Collections.Generic;
using AIFH_Vol3.Core.General.Data;

namespace AIFH_Vol3.Core.Learning.Score
{
    /// <summary>
    /// Score classification data. The score is the percentage cases that are wrong.
    /// There is no "partial credit" or closeness.  A case is either right or wrong.
    /// </summary>
    public class ScoreClassificationData : IScoreFunction
    {
        /// <summary>
        /// The training data.
        /// </summary>
        private readonly IList<BasicData> _trainingData;

        /// <summary>
        /// Construct the score function.
        /// </summary>
        /// <param name="theTrainingData">The training data.</param>
        public ScoreClassificationData(IList<BasicData> theTrainingData)
        {
            _trainingData = theTrainingData;
        }

        /// <inheritdoc/>
        public double CalculateScore(IMLMethod algo)
        {
            int incorrectCount = 0;
            int totalCount = 0;

            var ralgo = (IClassificationAlgorithm)algo;

            foreach (var aTrainingData in _trainingData)
            {
                totalCount++;
                var output = ralgo.ComputeClassification(aTrainingData.Input);

                if ( output != (int)aTrainingData.Ideal[0] )
                {
                    incorrectCount++;
                }
            }

            return (double)incorrectCount / totalCount;
        }
    }
}
