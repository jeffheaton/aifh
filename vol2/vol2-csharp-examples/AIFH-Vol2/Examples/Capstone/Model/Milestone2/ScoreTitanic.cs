// Artificial Intelligence for Humans
// Volume 2: Nature-Inspired Algorithms
// C# Version
// http://www.aifh.org
// http://www.jeffheaton.com
//
// Code repository:
// https://github.com/jeffheaton/aifh
//
// Copyright 2014 by Jeff Heaton
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
