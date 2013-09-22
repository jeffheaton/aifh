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

using System;
using System.Collections.Generic;
using AIFH_Vol1.Core.General.Data;
using MathNet.Numerics.LinearAlgebra.Double;
using MathNet.Numerics.LinearAlgebra.Generic;
using MathNet.Numerics.LinearAlgebra.Generic.Factorization;

namespace AIFH_Vol1.Core.Regression
{
    /// <summary>
    ///     Train a GLM using iteratively reweighted least squares.
    ///     http://en.wikipedia.org/wiki/Iteratively_reweighted_least_squares
    /// </summary>
    public class TrainReweightLeastSquares
    {
        /// <summary>
        ///     The GLM to train.
        /// </summary>
        private readonly MultipleLinearRegression _algorithm;

        /// <summary>
        ///     The gradient matrix.
        /// </summary>
        private readonly Matrix _gradient;

        /// <summary>
        ///     The Hessian matrix.
        /// </summary>
        private readonly Matrix<double> _hessian;

        /// <summary>
        ///     The training data.
        /// </summary>
        private readonly IList<BasicData> _trainingData;

        /// <summary>
        ///     The last error.
        /// </summary>
        private double _error;

        /// <summary>
        ///     Construct the trainer.
        /// </summary>
        /// <param name="theAlgorithm">The GLM to train.</param>
        /// <param name="theTrainingData">The training data.</param>
        public TrainReweightLeastSquares(MultipleLinearRegression theAlgorithm, IList<BasicData> theTrainingData)
        {
            _algorithm = theAlgorithm;
            _trainingData = theTrainingData;
            _gradient = new DenseMatrix(theAlgorithm.LongTermMemory.Length, 1);
            _hessian = new DenseMatrix(theAlgorithm.LongTermMemory.Length, theAlgorithm.LongTermMemory.Length);
        }

        /// <summary>
        ///     The last error.
        /// </summary>
        public double Error
        {
            get { return _error; }
        }

        /// <summary>
        ///     Perform one iteration of training.
        /// </summary>
        public void Iteration()
        {
            int rowCount = _trainingData.Count;
            int coeffCount = _algorithm.LongTermMemory.Length;

            var working = new double[rowCount, coeffCount];
            var errors = new double[rowCount];
            var weights = new double[rowCount];

            for (int i = 0; i < rowCount; i++)
            {
                BasicData element = _trainingData[i];

                working[i, 0] = 1;
                for (int j = 0; j < element.Input.Length; j++)
                    working[i, j + 1] = element.Input[j];
            }

            for (int i = 0; i < rowCount; i++)
            {
                BasicData element = _trainingData[i];
                double y = _algorithm.ComputeRegression(element.Input)[0];
                errors[i] = y - element.Ideal[0];
                weights[i] = y*(1.0 - y);
            }

            for (int i = 0; i < coeffCount; i++)
            {
                _gradient[i, 0] = 0;
                for (int j = 0; j < coeffCount; j++)
                    _hessian[i, j] = 0;
            }

            for (int j = 0; j < rowCount; j++)
            {
                for (int i = 0; i < coeffCount; i++)
                {
                    _gradient[i, 0] += working[j, i]*errors[j];
                }
            }

            for (int k = 0; k < weights.Length; k++)
            {
                for (int j = 0; j < coeffCount; j++)
                {
                    for (int i = 0; i < coeffCount; i++)
                    {
                        _hessian[j, i] += working[k,i]*working[k,j]*weights[k];
                    }
                }
            }

            LU<double> lu = _hessian.LU();

            Matrix<double> deltas = lu.Solve(_gradient);

            var prev = (double[]) _algorithm.LongTermMemory.Clone();

            for (int i = 0; i < _algorithm.LongTermMemory.Length; i++)
                _algorithm.LongTermMemory[i] -= deltas[i, 0];

            double max = 0;
            for (int i = 0; i < deltas.ColumnCount; i++)
                max = Math.Max(Math.Abs(deltas[i, 0])/Math.Abs(prev[i]), max);

            _error = max;
        }
    }
}