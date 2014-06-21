using AIFH_Vol2.Core.General.Data;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol2.Core.Learning.Score
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
        private IList<BasicData> _trainingData;

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

            IClassificationAlgorithm ralgo = (IClassificationAlgorithm)algo;

            foreach (BasicData aTrainingData in _trainingData)
            {
                totalCount++;
                int output = ralgo.ComputeClassification(aTrainingData.Input);

                if (output != (int)aTrainingData.Ideal[0])
                {
                    incorrectCount++;
                }
            }

            return (double)incorrectCount / (double)totalCount;
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
