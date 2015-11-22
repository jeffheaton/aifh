
namespace AIFH_Vol3.Core.Randomize
{
    /// <summary>
    /// Provides a foundation for most random number generation.  This allows the nextDouble to generate
    /// the other types.
    /// </summary>
    public abstract class AbstractGenerateRandom : IGenerateRandom
    {
        /// <inheritdoc/>
        public int NextInt(int low, int high)
        {
            return (low + (int)(NextDouble() * ((high - low))));
        }

        /// <inheritdoc/>
        public double NextDouble(double high)
        {
            return NextDouble(0, high);
        }

        /// <inheritdoc/>
        public double NextDouble(double low, double high)
        {
            return (low + (NextDouble() * ((high - low))));
        }

        /// <inheritdoc/>
        public int NextInt(int range)
        {
            return NextInt(0, range);
        }

        /// <inheritdoc/>
        public abstract double NextDouble();

        /// <inheritdoc/>
        public abstract bool NextBoolean();

        /// <inheritdoc/>
        public abstract float NextFloat();

        /// <inheritdoc/>
        public abstract double NextGaussian();

        /// <inheritdoc/>
        public abstract long NextLong();

        /// <inheritdoc/>
        public abstract int NextInt();
    }
}
