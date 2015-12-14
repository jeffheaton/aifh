using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3_Core.Core.ANN.Activation
{
    /// <summary>
    /// The Linear layer is really not an activation function at all. The input is
    /// simply passed on, unmodified, to the output.The linear activation function
    /// is usually used on the output layer of a regression network.
    /// </summary>
    public class ActivationLinear : IActivationFunction
    {
        /// <summary>
        /// Default empty parameters.
        /// </summary>
        public static readonly double[] P = new double[0];

        /// <summary>
        /// Default empty parameters.
        /// </summary>
        public static readonly string[] N = new String[0];

        /// <summary>
        /// The parameters.
        /// </summary>
        private readonly double[] _params;

        /// <summary>
        /// Construct a linear activation function, with a slope of 1.
        /// </summary>
        public ActivationLinear()
        {
            _params = new double[0];
        }

        public void ActivationFunction(double[] x, int start, int size)
        {
        }

        /// <summary>
        /// The object cloned.
        /// </summary>
        /// <returns>Cloned version.</returns>
        public IActivationFunction Clone()
        {
            return new ActivationLinear();
        }

        /// <inheritdoc/>
        public double DerivativeFunction(double b, double a)
        {
            return 1;
        }

        /// <inheritdoc/>
        public string[] ParamNames
        {
            get
            {
                string[] result = { };
                return result;
            }
        }

        /// <inheritdoc/>
        public double[] Params
        {
            get { return _params; }
        }

        /// <summary>
        /// Return true, linear has a 1 derivative.
        /// </summary>
        public bool HasDerivative
        {
            get { return true; }
        }

    }
}
