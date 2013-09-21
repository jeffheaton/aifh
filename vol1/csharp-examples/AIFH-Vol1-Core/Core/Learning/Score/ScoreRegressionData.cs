// Artificial Intelligence for Humans
// Volume 1: Fundamental Algorithms
// C# Version
// http://www.aifh.org
// http://www.jeffheaton.com
//
// Code repository:
// https://github.com/jeffheaton/aifh
//
// Copyright 2013 by Jeff Heaton
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
using AIFH_Vol1.Core.Error;
using AIFH_Vol1.Core.General.Data;

namespace AIFH_Vol1.Core.Learning.Score
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
        public double CalculateScore(IMachineLearningAlgorithm algo)
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
