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

using System;
using AIFH_Vol3.Core;

namespace AIFH_Vol3_Core.Core.ANN.Activation
{
    /// <summary>
    ///     The softmax activation function. This activation function is usually used on the output layer of
    ///     a classification network.
    /// </summary>
    public class ActivationSoftMax : IActivationFunction
    {
        /// <summary>
        ///     The parameters.
        /// </summary>
        private readonly double[] _params;

        /// <summary>
        ///     Construct the soft-max activation function.
        /// </summary>
        public ActivationSoftMax()
        {
            _params = new double[0];
        }

        /// <inheritdoc />
        public void ActivationFunction(double[] x, int start, int size)
        {
            double sum = 0;
            for (var i = start; i < start + size; i++)
            {
                x[i] = Math.Exp(x[i]);
                sum += x[i];
            }
            if (double.IsNaN(sum) || sum < AIFH.DefaultPrecision)
            {
                for (var i = start; i < start + size; i++)
                {
                    x[i] = 1.0/size;
                }
            }
            else
            {
                for (var i = start; i < start + size; i++)
                {
                    x[i] = x[i]/sum;
                }
            }
        }

        /// <inheritdoc />
        public IActivationFunction Clone()
        {
            return new ActivationSoftMax();
        }

        /// <inheritdoc />
        public double DerivativeFunction(double b, double a)
        {
            return a*(1.0 - a);
        }

        /// <inheritdoc />
        public string[] ParamNames
        {
            get
            {
                string[] result = {};
                return result;
            }
        }

        /// <inheritdoc />
        public double[] Params
        {
            get { return _params; }
        }

        /// <inheritdoc />
        public bool HasDerivative
        {
            get { return true; }
        }
    }
}