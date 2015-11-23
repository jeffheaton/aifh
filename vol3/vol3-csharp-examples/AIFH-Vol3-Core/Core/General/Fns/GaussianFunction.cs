
using System;

namespace AIFH_Vol3.Core.General.Fns
{
    /// <summary>
    /// The Gaussian function is a Radial Basis Function that describes the typical "bell curve", 
    /// or "normal distribution".
    ///
    /// The Gaussian function requires paramaters that specify the width (over all dimensions), as well as the
    /// centers of each dimension.  So a 3d Gaussian would have the parameters lined up as follows:
    ///
    /// params[0] = width (of all dimensions),
    ///
    /// params[1] = center of dimension 0,
    /// 
    /// params[2] = center of dimension 1,
    /// 
    /// params[3] = center of dimension 3
    /// 
    /// http://en.wikipedia.org/wiki/Gaussian_function
    /// </summary>
    public class GaussianFunction : AbstractRBF
    {
        /// <summary>
        /// Construct the Gaussian RBF. Each RBF will require space equal to (dimensions + 1) in the params 
        /// vector.
        /// </summary>
        /// <param name="theDimensions">The number of dimensions.</param>
        /// <param name="theParams">A vector to hold the parameters.</param>
        /// <param name="theIndex">The index into the params vector.  You can store multiple RBF's in a 
        /// vector.</param>
        public GaussianFunction(int theDimensions, double[] theParams, int theIndex)
            : base(theDimensions, theParams, theIndex)
        {

        }

        /// <inheritdoc/>
        public override double Evaluate(double[] x)
        {
            double value = 0;
            double width = Width;

            for (int i = 0; i < Dimensions; i++)
            {
                double center = GetCenter(i);
                value += Math.Pow(x[i] - center, 2) / (2.0 * width * width);
            }
            return Math.Exp(-value);
        }
    }
}
