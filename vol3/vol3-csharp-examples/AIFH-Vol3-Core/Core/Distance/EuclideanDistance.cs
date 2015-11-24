using System;

namespace AIFH_Vol3.Core.Distance
{
    /// <summary>
    /// The Euclidean distance is the straight-line distance between two points.  It is calculated by taking the
    /// square root of the sum of squares differences between each point in the vector.
    /// 
    /// http://www.heatonresearch.com/wiki/Euclidean_Distance
    /// </summary>
    public class EuclideanDistance : AbstractDistance
    {
        /// <inheritdoc/>
        public override double Calculate(double[] position1, int pos1, double[] position2, int pos2, int length)
        {
            double sum = 0;
            for (int i = 0; i < length; i++)
            {
                double d = position1[i + pos1] - position2[i + pos1];
                sum += d * d;
            }
            return Math.Sqrt(sum);
        }
    }
}
