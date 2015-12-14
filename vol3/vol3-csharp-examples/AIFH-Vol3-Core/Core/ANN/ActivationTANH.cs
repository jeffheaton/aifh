using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using AIFH_Vol3_Core.Core.ANN.Activation;

namespace AIFH_Vol3_Core.Core.ANN
{
    /// <summary>
    /// The hyperbolic tangent activation function takes the curved shape of the
    /// hyperbolic tangent.This activation function produces both positive and
    /// negative output.Use this activation function if both negative and positive
    /// output is desired.
    /// </summary>
    public class ActivationTANH : IActivationFunction
    {
        /// <summary>
        /// The parameters.
        /// </summary>
        private double[] _params;

        /// <summary>
        /// Construct a basic HTAN activation function, with a slope of 1.
        /// </summary>
        public ActivationTANH()
        {
            _params = new double[0];
        }

        /// <inheritdoc/>
        public void ActivationFunction(double[] x, int start, int size)
        {
            for (int i = start; i < start + size; i++)
            {
                x[i] = Math.Tanh(x[i]);
            }
        }

        /// <inheritdoc/>
        public IActivationFunction Clone()
        {
            return new ActivationTANH();
        }

        /// <inheritdoc/>
        public double DerivativeFunction(double b, double a)
        {
            return (1.0 - a * a);
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

        /// <inheritdoc/>
        public bool HasDerivative
        {
            get { return true; }
        }

    }
}
