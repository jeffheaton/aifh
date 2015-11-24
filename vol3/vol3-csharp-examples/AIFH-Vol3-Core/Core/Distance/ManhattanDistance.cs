using System;

namespace AIFH_Vol3.Core.Distance
{
    /// <summary>
    /// The Manhattan Distance (also known as Taxicab distance) is a Distance Metric used in machine learning.
    /// This distance is used to compare how similar two vectors of uniform length are. A lower length indicates that
    /// the two vectors are more similar than two vectors with a larger length.
    /// 
    /// http://www.heatonresearch.com/wiki/Manhattan_Distance
    /// </summary>
    public class ManhattanDistance : AbstractDistance
    {
        /// <inheritdoc/>
        public override double Calculate(double[] position1, int pos1, double[] position2, int pos2, int length)
        {
            double sum = 0;
            for (int i = 0; i < position1.Length; i++)
            {
                double d = Math.Abs(position1[pos1+i] - position2[pos2+i]);
                sum += d;
            }
            return sum;
        }
    }
}
