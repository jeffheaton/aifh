
using System;

namespace AIFH_Vol3.Core.Randomize
{
    /// <summary>
    /// A wrapper over C#'s built in random number generator.
    /// </summary>
    public class BasicGenerateRandom : AbstractBoxMuller
    {
        /// <summary>
        /// The random number generator.
        /// </summary>
        protected readonly Random _rnd;

        /// <summary>
        /// Construct a random number generator with the specified seed. 
        /// </summary>
        /// <param name="seed">The seed.</param>
        public BasicGenerateRandom(int seed)
        {
            _rnd = new Random(seed);
        }

        /// <summary>
        /// Construct a random number generator with a time-based seed.
        /// </summary>
        public BasicGenerateRandom()
        {
            _rnd = new Random();
        }

        /// <inheritdoc/>
        public override double NextDouble()
        {
            return _rnd.NextDouble();
        }

        /// <inheritdoc/>
        public override bool NextBoolean()
        {
            return _rnd.NextDouble() > 0.5;
        }

        /// <inheritdoc/>
        public override float NextFloat()
        {
            return (float)_rnd.NextDouble();
        }

        /// <inheritdoc/>
        public override long NextLong()
        {
            return _rnd.Next();
        }

        /// <inheritdoc/>
        public override int NextInt()
        {
            return _rnd.Next();
        }
    }
}
