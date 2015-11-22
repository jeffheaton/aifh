
using System;

namespace AIFH_Vol3.Core.Randomize
{
    /// <summary>
    /// Provides the ability for subclasses to generate normally distributed random numbers.
    /// </summary>
    public abstract class AbstractBoxMuller : AbstractGenerateRandom
    {
        /// <summary>
        /// The y2 value.
        /// </summary>
        private double _y2;

        /// <summary>
        /// Should we use the last value.
        /// </summary>
        private bool _useLast;

        /// <summary>
        /// The mean.
        /// </summary>
        public const double Mu = 0;

        /// <summary>
        /// The standard deviation.
        /// </summary>
        private const double Sigma = 1;


        /// <inheritdoc/>
        public override double NextGaussian()
        {
            double y1;

            // use value from previous call
            if (_useLast)
            {
                y1 = _y2;
                _useLast = false;
            }
            else
            {
                double x1;
                double x2;
                double w;
                do
                {
                    x1 = 2.0 * NextDouble() - 1.0;
                    x2 = 2.0 * NextDouble() - 1.0;
                    w = x1 * x1 + x2 * x2;
                } while (w >= 1.0);

                w = Math.Sqrt((-2.0 * Math.Log(w)) / w);
                y1 = x1 * w;
                _y2 = x2 * w;
                _useLast = true;
            }

            return (Mu + y1 * Sigma);
        }

        /// <inheritdoc/>
        public abstract override double NextDouble();

        /// <inheritdoc/>
        public abstract override bool NextBoolean();

        /// <inheritdoc/>
        public abstract override float NextFloat();

        /// <inheritdoc/>
        public abstract override long NextLong();

        /// <inheritdoc/>
        public abstract override int NextInt();

    }
}
