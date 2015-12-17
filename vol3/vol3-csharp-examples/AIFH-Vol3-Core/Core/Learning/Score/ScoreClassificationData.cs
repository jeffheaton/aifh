// Artificial Intelligence for Humans
// Volume 3: Deep Learning and Neural Networks
// C# Version
// http://www.aifh.org
// http://www.jeffheaton.com
//
// Code repository:
// https://github.com/jeffheaton/aifh
//
// Copyright 2015 by Jeff Heaton
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// For more information on Heaton Research copyrights, licenses
// and trademarks visit:
// http://www.heatonresearch.com/copyright
//

using System.Collections.Generic;
using AIFH_Vol3.Core.General.Data;

namespace AIFH_Vol3.Core.Learning.Score
{
    /// <summary>
    ///     Score classification data. The score is the percentage cases that are wrong.
    ///     There is no "partial credit" or closeness.  A case is either right or wrong.
    /// </summary>
    public class ScoreClassificationData : IScoreFunction
    {
        /// <summary>
        ///     The training data.
        /// </summary>
        private readonly IList<BasicData> _trainingData;

        /// <summary>
        ///     Construct the score function.
        /// </summary>
        /// <param name="theTrainingData">The training data.</param>
        public ScoreClassificationData(IList<BasicData> theTrainingData)
        {
            _trainingData = theTrainingData;
        }

        /// <inheritdoc />
        public double CalculateScore(IMLMethod algo)
        {
            var incorrectCount = 0;
            var totalCount = 0;

            var ralgo = (IClassificationAlgorithm) algo;

            foreach (var aTrainingData in _trainingData)
            {
                totalCount++;
                var output = ralgo.ComputeClassification(aTrainingData.Input);

                if (output != (int) aTrainingData.Ideal[0])
                {
                    incorrectCount++;
                }
            }

            return (double) incorrectCount/totalCount;
        }
    }
}