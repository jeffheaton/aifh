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
    ///     A Rectified Linear Unit(ReLU activation function.This activation function is commonly
    ///     used for hidden layers of a neural network.  A ReLU activation function will usually
    ///     perform better than tanh and sigmoid.  This is the most popular activation function for
    ///     deep neural networks.
    ///     Glorot, X., Bordes, A., & Bengio, Y. (2011). Deep sparse rectifier neural networks.In International Conference
    ///     on Artificial Intelligence and Statistics(pp. 315-323).
    /// </summary>
    public class ActivationReLU : IActivationFunction
    {
        /// <summary>
        ///     The ramp low threshold parameter.
        /// </summary>
        public const int PARAM_RELU_LOW_THRESHOLD = 0;

        /// <summary>
        ///     The ramp low parameter.
        /// </summary>
        public const int PARAM_RELU_LOW = 0;


        /// <summary>
        ///     The parameters.
        /// </summary>
        private readonly double[] _params;

        /// <summary>
        ///     Default constructor.
        /// </summary>
        public ActivationReLU() : this(0, 0)
        {
        }

        /// <summary>
        ///     Construct a ramp activation function.
        /// </summary>
        /// <param name="thresholdLow">The low threshold value.</param>
        /// <param name="low">The low value, replaced if the low threshold is exceeded.</param>
        public ActivationReLU(double thresholdLow, double low)
        {
            _params = new double[2];
            _params[PARAM_RELU_LOW_THRESHOLD] = thresholdLow;
            _params[PARAM_RELU_LOW] = low;
        }

        /// <inheritdoc />
        public double Low
        {
            get { return _params[PARAM_RELU_LOW]; }
            set { _params[PARAM_RELU_LOW] = value; }
        }

        /// <summary>
        ///     The threshold low.
        /// </summary>
        public double ThresholdLow
        {
            get { return _params[PARAM_RELU_LOW_THRESHOLD]; }
            set { _params[PARAM_RELU_LOW_THRESHOLD] = value; }
        }

        /// <inheritdoc />
        public void ActivationFunction(double[] x, int start, int size)
        {
            for (var i = start; i < start + size; i++)
            {
                if (x[i] <= _params[PARAM_RELU_LOW_THRESHOLD])
                {
                    x[i] = _params[PARAM_RELU_LOW];
                }
            }
        }

        /// <summary>
        ///     Clone the object.
        /// </summary>
        /// <returns>The cloned object.</returns>
        public IActivationFunction Clone()
        {
            return new ActivationReLU(
                _params[PARAM_RELU_LOW_THRESHOLD],
                _params[PARAM_RELU_LOW]);
        }

        /// <inheritdoc />
        public double DerivativeFunction(double b, double a)
        {
            if (b <= _params[PARAM_RELU_LOW_THRESHOLD])
            {
                return 0;
            }
            return 1.0;
        }

        /// <inheritdoc />
        public string[] ParamNames
        {
            get
            {
                string[] result = {"thresholdLow", "low"};
                return result;
            }
        }

        /// <inheritdoc />
        public double[] Params
        {
            get { return _params; }
        }

        /// <summary>
        ///     True, as this function does have a derivative.
        /// </summary>
        public bool HasDerivative
        {
            get { return true; }
        }
    }
}