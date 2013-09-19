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

using AIFH_Vol1.Core.General.Fns;
using AIFH_Vol1.Core.General.Fns.Link;
using AIFH_Vol1.Core.Learning;

namespace AIFH_Vol1.Core.Regression
{
    /// <summary>
    ///     Implements a multi-input linear regression function, with an optional link function.  By default the link function
    ///     is the identity function, which implements regular linear regression.  Setting the link function to other function
    ///     types allows you to create other Generalized Linear Models(GLMs).
    ///     The long term memory is always of one greater length than the number of inputs.  The first memory element is the
    ///     intercept, and the others are coefficients to the inputs.
    ///     For simple Linear Regression you should train with TrainLeastSquares.  If you are using a GLM, then you must
    ///     train with Reweight Least Squares.
    ///     http://en.wikipedia.org/wiki/Linear_regression
    ///     http://en.wikipedia.org/wiki/Generalized_linear_model
    /// </summary>
    public class MultipleLinearRegression : IRegressionAlgorithm
    {
        /// <summary>
        ///     The long term memory, in this case coefficients to the linear regression.
        /// </summary>
        private readonly double[] _longTermMemory;

        /// <summary>
        ///     The link function to use.
        /// </summary>
        private IFn _linkFunction = new IdentityLinkFunction();

        public MultipleLinearRegression(int theInputCount)
        {
            _longTermMemory = new double[theInputCount + 1];
        }

        /// <summary>
        ///     The link function.
        /// </summary>
        public IFn LinkFunction
        {
            get { return _linkFunction; }
            set { _linkFunction = value; }
        }

        /// <inheritdoc />
        public double[] ComputeRegression(double[] input)
        {
            double sum = 0;

            for (int i = 1; i < _longTermMemory.Length; i++)
            {
                sum += input[i - 1]*_longTermMemory[i];
            }
            sum += _longTermMemory[0];

            var result = new double[1];
            result[0] = sum;
            result[0] = _linkFunction.Evaluate(result);
            return result;
        }

        /// <inheritdoc />
        public double[] LongTermMemory
        {
            get { return _longTermMemory; }
        }
    }
}