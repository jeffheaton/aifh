
namespace AIFH_Vol3.Core.Distance
{
    /// <summary>
    /// This distance calculator provides a convenience calculation of vectors with 0-based index.
    /// </summary>
    public abstract class AbstractDistance : ICalculateDistance
    {
        /// <inheritdoc/>
        public double Calculate(double[] position1, double[] position2)
        {
            return Calculate(position1, 0, position2, 0, position1.Length);
        }

        /// <inheritdoc/>
        public abstract double Calculate(double[] position1, int pos1, double[] position2, int pos2, int length);
    }
}
