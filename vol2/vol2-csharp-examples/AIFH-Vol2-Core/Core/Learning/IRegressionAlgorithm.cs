using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol2.Core.Learning
{
    /// <summary>
    /// A regression algorithm provides an output for the given input.  This allows the machine learning algorithm to
    /// approximate a function.
    /// </summary>
    public interface IRegressionAlgorithm: IMLMethod
    {
        /// <summary>
        /// Compute the output for the specified input.
        /// </summary>
        /// <param name="input">The input.</param>
        /// <returns>The regression output.</returns>
        double[] ComputeRegression(double[] input);
    }
}
