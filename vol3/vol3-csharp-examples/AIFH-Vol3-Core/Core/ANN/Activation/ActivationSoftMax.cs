using System;
using AIFH_Vol3.Core;

namespace AIFH_Vol3_Core.Core.ANN.Activation
{
    /// <summary>
    /// The softmax activation function. This activation function is usually used on the output layer of 
    /// a classification network.
    /// </summary>
    public class ActivationSoftMax : IActivationFunction
    {
        /// <summary>
        /// The parameters.
        /// </summary>
        private double[] _params;

        /// <summary>
        /// Construct the soft-max activation function.
        /// </summary>
        public ActivationSoftMax()
        {
            _params = new double[0];
        }

        /// <inheritdoc/>
        public void ActivationFunction(double[] x, int start, int size)
        {
            double sum = 0;
            for (int i = start; i < start + size; i++)
            {
                x[i] = Math.Exp(x[i]);
                sum += x[i];
            }
            if (double.IsNaN(sum) || sum < AIFH.DefaultPrecision)
            {
                for (int i = start; i < start + size; i++)
                {
                    x[i] = 1.0 / size;
                }
            }
            else
            {
                for (int i = start; i < start + size; i++)
                {
                    x[i] = x[i] / sum;
                }
            }
        }

        /// <inheritdoc/>
        public IActivationFunction Clone()
        {
            return new ActivationSoftMax();
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
