using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3_Core.Core.ANN.Activation
{
    /// <summary>
    /// A Rectified Linear Unit(ReLU activation function.This activation function is commonly
    /// used for hidden layers of a neural network.  A ReLU activation function will usually
    /// perform better than tanh and sigmoid.  This is the most popular activation function for
    /// deep neural networks.
    ///
    /// Glorot, X., Bordes, A., & Bengio, Y. (2011). Deep sparse rectifier neural networks.In International Conference
    /// on Artificial Intelligence and Statistics(pp. 315-323).
    /// </summary>
    public class ActivationReLU : IActivationFunction
    {
        /// <summary>
        /// The ramp low threshold parameter.
        /// </summary>
        public const int PARAM_RELU_LOW_THRESHOLD = 0;

        /// <summary>
        /// The ramp low parameter.
        /// </summary>
        public const int PARAM_RELU_LOW = 0;


        /// <summary>
        /// The parameters.
        /// </summary>
        private double[] _params;

        /// <summary>
        /// Default constructor.
        /// </summary>
        public ActivationReLU() : this(0, 0)
        {

        }
        
        /// <summary>
        /// Construct a ramp activation function. 
        /// </summary>
        /// <param name="thresholdLow">The low threshold value.</param>
        /// <param name="low">The low value, replaced if the low threshold is exceeded.</param>
        public ActivationReLU(double thresholdLow, double low)
        {
            _params = new double[2];
            _params[ActivationReLU.PARAM_RELU_LOW_THRESHOLD] = thresholdLow;
            _params[ActivationReLU.PARAM_RELU_LOW] = low;
        }

        /// <inheritdoc/>
        public void ActivationFunction(double[] x, int start, int size)
        {
            for (int i = start; i < start + size; i++)
            {
                if (x[i] <= _params[ActivationReLU.PARAM_RELU_LOW_THRESHOLD])
                {
                    x[i] = _params[ActivationReLU.PARAM_RELU_LOW];
                }
            }

        }
        
        /// <summary>
        /// Clone the object.
        /// </summary>
        /// <returns>The cloned object.</returns>
        public IActivationFunction Clone()
        {
            return new ActivationReLU(
                _params[ActivationReLU.PARAM_RELU_LOW_THRESHOLD],
                _params[ActivationReLU.PARAM_RELU_LOW]);
        }

        /// <inheritdoc/>
        public double DerivativeFunction(double b, double a)
        {
            if (b <= _params[ActivationReLU.PARAM_RELU_LOW_THRESHOLD])
            {
                return 0;
            }
            return 1.0;
        }

        /// <inheritdoc/>
        public double Low
        {
            get { return _params[ActivationReLU.PARAM_RELU_LOW]; }
            set { _params[ActivationReLU.PARAM_RELU_LOW] = value; }
        }

        /// <inheritdoc/>
        public string[] ParamNames
        {
            get
            {
                string[] result = {"thresholdLow", "low"};
                return result;
            }
        }

        /// <inheritdoc/>
        public double[] Params
        {
            get { return _params; }
        }

        /// <summary>
        /// The threshold low.
        /// </summary>

        public double ThresholdLow
        {
            get { return _params[ActivationReLU.PARAM_RELU_LOW_THRESHOLD]; }
            set { _params[ActivationReLU.PARAM_RELU_LOW_THRESHOLD] = value; }
        }

        /// <summary>
        /// True, as this function does have a derivative.
        /// </summary>
        public bool HasDerivative
        {
            get { return true; }

        }
    }
}
