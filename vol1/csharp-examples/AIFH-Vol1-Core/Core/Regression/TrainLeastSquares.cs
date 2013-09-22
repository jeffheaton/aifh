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
using System.Linq;
using AIFH_Vol1.Core.Error;
using AIFH_Vol1.Core.General.Data;
using MathNet.Numerics.LinearAlgebra.Double;
using MathNet.Numerics.LinearAlgebra.Double.Factorization;
using MathNet.Numerics.LinearAlgebra.Generic;
using MathNet.Numerics.LinearAlgebra.Generic.Factorization;

namespace AIFH_Vol1.Core.Regression
{
    /// <summary>
    ///     Train a Linear Regression with Least Squares.  This will only work if you use the default identity function.
    ///     Note:, if you get this error message.
    ///     java.lang.RuntimeException: Matrix is rank deficient.
    ///     It means that a linear regression cannot be fit to your data.
    /// </summary>
    public class TrainLeastSquares
    {
        /// <summary>
        ///     The linear regression object we are training.
        /// </summary>
        private readonly MultipleLinearRegression _algorithm;

        /// <summary>
        ///     An error calculation method.
        /// </summary>
        private readonly IErrorCalculation _errorCalculation = new ErrorCalculationMSE();

        /// <summary>
        ///     The training data.
        /// </summary>
        private readonly IList<BasicData> _trainingData;

        /// <summary>
        ///     The last error.
        /// </summary>
        private double _error;

        /// <summary>
        ///     Sum of squares for error.
        /// </summary>
        private double _sse;

        /// <summary>
        ///     Total sum of squares.
        /// </summary>
        private double _sst;

        /// <summary>
        ///     Construct the trainer.
        /// </summary>
        /// <param name="theAlgorithm">The algorithm to train.</param>
        /// <param name="theTrainingData">The training data.</param>
        public TrainLeastSquares(MultipleLinearRegression theAlgorithm, IList<BasicData> theTrainingData)
        {
            _algorithm = theAlgorithm;
            _trainingData = theTrainingData;
        }

        /// <summary>
        ///     The R squared value.  The coefficient of determination.
        /// </summary>
        public double R2
        {
            get { return 1.0 - _sse/_sst; }
        }

        /// <summary>
        ///     The current error.
        /// </summary>
        public double Error
        {
            get { return _error; }
        }

        /// <summary>
        ///     Train.  Single iteration.
        /// </summary>
        public void Iteration()
        {
            int rowCount = _trainingData.Count;
            int inputColCount = _trainingData[0].Input.Length;

            Matrix<double> xMatrix = new DenseMatrix(rowCount, inputColCount + 1);
            Matrix<double> yMatrix = new DenseMatrix(rowCount, 1);

            for (int row = 0; row < _trainingData.Count; row++)
            {
                BasicData dataRow = _trainingData[row];
                int colSize = dataRow.Input.Count();

                xMatrix[row, 0] = 1;
                for (int col = 0; col < colSize; col++)
                {
                    xMatrix[row, col + 1] = dataRow.Input[col];
                }
                yMatrix[row, 0] = dataRow.Ideal[0];
            }

            // Calculate the least squares solution
            QR qr = xMatrix.QR();
            Matrix<double> beta = qr.Solve(yMatrix);

            double sum = 0.0;
            for (int i = 0; i < inputColCount; i++)
                sum += yMatrix[i, 0];
            double mean = sum/inputColCount;

            for (int i = 0; i < inputColCount; i++)
            {
                double dev = yMatrix[i, 0] - mean;
                _sst += dev*dev;
            }

            Matrix<double> residuals = xMatrix.Multiply(beta).Subtract(yMatrix);
            _sse = residuals.L2Norm()*residuals.L2Norm();

            for (int i = 0; i < _algorithm.LongTermMemory.Length; i++)
            {
                _algorithm.LongTermMemory[i] = beta[i, 0];
            }

            // calculate error
            _errorCalculation.Clear();
            foreach (BasicData dataRow in _trainingData)
            {
                double[] output = _algorithm.ComputeRegression(dataRow.Input);
                _errorCalculation.UpdateError(output, dataRow.Ideal, 1.0);
            }
            _error = _errorCalculation.Calculate();
        }
    }
}