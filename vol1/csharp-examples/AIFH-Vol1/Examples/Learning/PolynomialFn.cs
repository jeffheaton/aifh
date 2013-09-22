using System;
using System.Linq;
using System.Text;
using AIFH_Vol1.Core.Learning;

namespace AIFH_Vol1.Examples.Learning
{
    /// <summary>
    ///     A simple polynomial function, of a specified order. Implemented as a regression algorithm.
    ///     http://en.wikipedia.org/wiki/Polynomial
    /// </summary>
    public class PolynomialFn: IRegressionAlgorithm
    {
        /// <summary>
        ///     The coefficients.  The first is the intercept.
        /// </summary>
        private readonly double[] _longTermMemory;

        /// <summary>
        ///     Construct a polynomial function.
        /// </summary>
        /// <param name="polyOrder">The order.</param>
        public PolynomialFn(int polyOrder)
        {
            _longTermMemory = new double[polyOrder];
        }

        /// <inheritdoc />
        public double[] ComputeRegression(double[] input)
        {
            double x = input[0];
            double y = _longTermMemory.Select((t, i) => t*Math.Pow(x, i)).Sum();

            var result = new double[1];
            result[0] = y;

            return result;
        }

        /// <inheritdoc />
        public double[] LongTermMemory
        {
            get
            {
                return _longTermMemory;
            }
        }

        /// <inheritdoc />
        public override String ToString()
        {
            var result = new StringBuilder();

            for (int i = _longTermMemory.Length - 1; i >= 0; i--)
            {
                double c = _longTermMemory[i];

                if (result.Length > 0)
                {
                    if (c >= 0)
                    {
                        result.Append('+');
                    }
                }

                result.Append(c);

                if (i >= 2)
                {
                    result.Append("x^");
                    result.Append(i);
                }
                else if (i >= 1)
                {
                    result.Append("x");
                }
            }

            return result.ToString();
        }
    }
}