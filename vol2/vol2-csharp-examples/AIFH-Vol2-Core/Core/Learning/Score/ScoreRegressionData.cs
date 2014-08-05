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
