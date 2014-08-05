// Artificial Intelligence for Humans
// Volume 2: Nature-Inspired Algorithms
// C# Version
// http://www.aifh.org
// http://www.jeffheaton.com
//
// Code repository:
// https://github.com/jeffheaton/aifh
//
// Copyright 2014 by Jeff Heaton
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// For more information on Heaton Research copyrights, licenses
// and trademarks visit:
// http://www.heatonresearch.com/copyright
//
using System;
using System.Security.Cryptography;

namespace AIFH_Vol2.Core.Randomize
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
