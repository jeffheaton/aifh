
using System;
using System.Security.Cryptography;

namespace AIFH_Vol3.Core.Randomize
{
    public class SecureGenerateRandom : AbstractBoxMuller
    {
        /// <summary>
        /// The random number generator.
        /// </summary>
        private readonly RandomNumberGenerator _rnd; 

        /// <summary>
        /// Construct a random number generator with a time-based seed.
        /// </summary>
        public SecureGenerateRandom()
        {
            _rnd = RandomNumberGenerator.Create();
        }

        /// <inheritdoc/>
        public override double NextDouble()
        {
            var result = new byte[8];
            _rnd.GetBytes(result);
            return (double)BitConverter.ToUInt64(result, 0) / ulong.MaxValue;
        }

        /// <inheritdoc/>
        public override bool NextBoolean()
        {
            return NextDouble() > 0.5;
        }

        /// <inheritdoc/>
        public override float NextFloat()
        {
            var result = new byte[4];
            _rnd.GetBytes(result);
            return (float)BitConverter.ToUInt32(result, 0) / ulong.MaxValue;
        }

        /// <inheritdoc/>
        public override long NextLong()
        {
            var result = new byte[8];
            _rnd.GetBytes(result);
            return (long)BitConverter.ToUInt64(result, 0);
        }

        /// <inheritdoc/>
        public override int NextInt()
        {
            var result = new byte[4];
            _rnd.GetBytes(result);
            return (int)BitConverter.ToUInt32(result, 0);
        }
    }
}
