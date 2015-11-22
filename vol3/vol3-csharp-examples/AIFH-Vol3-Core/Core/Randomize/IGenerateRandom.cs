
namespace AIFH_Vol3.Core.Randomize
{
    /// <summary>
    /// Interface that defines how random numbers are generated.  Provides the means to generate both uniform 
    /// and normal (gaussian) distributed random numbers.
    /// </summary>
    public interface IGenerateRandom
    {
        /// <summary>
        /// Generate the next normally distributed random number.
        /// </summary>
        /// <returns>The next normally distributed random number.</returns>
        double NextGaussian();

        /// <summary>
        /// Generate the next random boolean.
        /// </summary>
        /// <returns>The next random boolean.</returns>
        bool NextBoolean();

        /// <summary>
        /// Generate the next random long.
        /// </summary>
        /// <returns>The next random long.</returns>
        long NextLong();

        /// <summary>
        /// Generate the next random floating point.
        /// </summary>
        /// <returns>The next random floating point.</returns>
        float NextFloat();

        /// <summary>
        /// Generate the next random double.
        /// </summary>
        /// <returns>The next random double.</returns>
        double NextDouble();

        /// <summary>
        /// The next random double up to a non-inclusive range. 
        /// </summary>
        /// <param name="high">The highest desired value.</param>
        /// <returns>The result.</returns>
        double NextDouble(double high);

        /// <summary>
        /// The next double between low (inclusive) and high (exclusive). 
        /// </summary>
        /// <param name="low">The inclusive low value.</param>
        /// <param name="high">The exclusive high value.</param>
        /// <returns>The result.</returns>
        double NextDouble(double low, double high);

        /// <summary>
        /// Generate the next random integer.
        /// </summary>
        /// <returns>The next random integer.</returns>
        int NextInt();

        /// <summary>
        /// The next random int up to a non-inclusive range. 
        /// </summary>
        /// <param name="high">The highest desired value.</param>
        /// <returns>The result.</returns>
        int NextInt(int high);

        /// <summary>
        /// The next int between low (inclusive) and high (exclusive). 
        /// </summary>
        /// <param name="low">The inclusive low value.</param>
        /// <param name="high">The exclusive high value.</param>
        /// <returns>The result.</returns>
        int NextInt(int low, int high);
    }
}
