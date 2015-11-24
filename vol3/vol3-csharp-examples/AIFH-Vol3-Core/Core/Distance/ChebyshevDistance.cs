
using System;

namespace AIFH_Vol3.Core.Distance
{
    /// <summary>
    /// Chebyshev distance is the maximum absolute difference between any two vector elements.  This can be thought
    /// of as the number of spaces that a king chess piece must travel between two squares in a 2D dimension space.
    ///
    /// http://www.heatonresearch.com/wiki/Chebyshev_Distance
    /// </summary>
    public class ChebyshevDistance : AbstractDistance
    {
        /// <inheritdoc/>
        public override double Calculate(double[] position1, int pos1, double[] position2, int pos2, int length)
        {
            double result = 0;
            for (int i = 0; i < length; i++)
            {
                double d = Math.Abs(position1[pos1 + i] - position2[pos2 + i]);
                result = Math.Max(d, result);
            }
            return result;
        }

    }
}
