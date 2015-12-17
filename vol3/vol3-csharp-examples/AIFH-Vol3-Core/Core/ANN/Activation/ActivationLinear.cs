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
namespace AIFH_Vol3_Core.Core.ANN.Activation
{
    /// <summary>
    ///     The Linear layer is really not an activation function at all. The input is
    ///     simply passed on, unmodified, to the output.The linear activation function
    ///     is usually used on the output layer of a regression network.
    /// </summary>
    public class ActivationLinear : IActivationFunction
    {
        /// <summary>
        ///     Default empty parameters.
        /// </summary>
        public static readonly double[] P = new double[0];

        /// <summary>
        ///     Default empty parameters.
        /// </summary>
        public static readonly string[] N = new string[0];

        /// <summary>
        ///     The parameters.
        /// </summary>
        private readonly double[] _params;

        /// <summary>
        ///     Construct a linear activation function, with a slope of 1.
        /// </summary>
        public ActivationLinear()
        {
            _params = new double[0];
        }

        public void ActivationFunction(double[] x, int start, int size)
        {
        }

        /// <summary>
        ///     The object cloned.
        /// </summary>
        /// <returns>Cloned version.</returns>
        public IActivationFunction Clone()
        {
            return new ActivationLinear();
        }

        /// <inheritdoc />
        public double DerivativeFunction(double b, double a)
        {
            return 1;
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

        /// <summary>
        ///     Return true, linear has a 1 derivative.
        /// </summary>
        public bool HasDerivative
        {
            get { return true; }
        }
    }
}