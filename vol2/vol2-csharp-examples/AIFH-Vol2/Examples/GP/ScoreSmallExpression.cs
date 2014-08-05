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
