using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3_Core.Core.ANN.Activation
{
    /// <summary>
    /// The sigmoid activation function takes on a sigmoidal shape. Only positive
    /// numbers are generated.Do not use this activation function if negative number
    /// output is desired.
    /// </summary>
    public class ActivationSigmoid:IActivationFunction
    {
        /// <summary>
        /// The parameters.
        /// </summary>
        private readonly double[] _params;

        /// <summary>
        /// Construct a basic sigmoid function, with a slope of 1.
        /// </summary>
        public ActivationSigmoid()
        {
            _params = new double[0];
        }

        /// <inheritdoc/>
        public void ActivationFunction(double[] x, int start, int size)
        {
            for (int i = start; i < start + size; i++)
            {
                x[i] = 1.0 / (1.0 + Math.Exp(-1 * x[i]));
            }
        }

        /// <inheritdoc/>
        public IActivationFunction Clone()
        {
            return new ActivationSigmoid();
        }

        /// <inheritdoc/>
        public double DerivativeFunction(double b, double a)
        {
            return a * (1.0 - a);
        }

        /// <inheritdoc/>
        public string[] ParamNames
        {
            get
            {
                string[] results = {};
                return results;
            }
        }

        /// <inheritdoc/>
        public double[] Params
        {
            get { return _params; }
        }

        /// <summary>
        /// True, sigmoid has a derivative.
        /// </summary>
        public bool HasDerivative
        {
            get { return true; }
        } 
    }
}
