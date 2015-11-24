
namespace AIFH_Vol3.Core.Distance
{
    /// <summary>
    /// Calculate the distance between two vectors.  These vectors are double arrays.
    /// Both vectors must be of the same length. These two vectors can be thought of
    /// positions in a potentially high dimension space.
    /// 
    /// You can think of a distance metric as measuring the similarity between two vectors.
    /// </summary>
    public interface ICalculateDistance
    {
        /// <summary>
        /// Calculate the distance between two vectors, both full length and starting at position 0.
        /// </summary>
        /// <param name="position1">The first vector.</param>
        /// <param name="position2">The second vector.</param>
        /// <returns>The distance.</returns>
        double Calculate(double[] position1, double[] position2);

        /// <summary>
        /// Calculate the distance between two vectors. 
        /// </summary>
        /// <param name="position1">The first vector.</param>
        /// <param name="pos1">The position to use in the first vector.</param>
        /// <param name="position2">The second vector.</param>
        /// <param name="pos2">The position to use in the second vector.</param>
        /// <param name="length">The length of both vectors, they must be the same.</param>
        /// <returns>The distance.</returns>
        double Calculate(double[] position1, int pos1, double[] position2, int pos2, int length);
    }
}
