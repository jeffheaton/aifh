
using System;
namespace AIFH_Vol3.Core.General.Fns
{
    /// <summary>
    /// The Multiquadric Radial Basis Function.
    ///
    /// http://en.wikipedia.org/wiki/Radial_basis_function
    /// </summary>
    public class MultiquadricFunction : AbstractRBF
    {
        /// <summary>
        /// Construct the Multiquadric RBF. Each RBF will require space equal to (dimensions + 1) in the params vector.
        /// </summary>
        /// <param name="theDimensions">The number of dimensions.</param>
        /// <param name="theParams">A vector to hold the parameters.</param>
        /// <param name="theIndex">The index into the params vector.  You can store multiple RBF's in a vector.</param>
        public MultiquadricFunction(int theDimensions, double[] theParams, int theIndex)
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
                value += Math.Pow(x[i] - center, 2) + (width * width);
            }
            return Math.Sqrt(value);

        }
    }
}
