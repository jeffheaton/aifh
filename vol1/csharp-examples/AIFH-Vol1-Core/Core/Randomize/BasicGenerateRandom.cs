// Artificial Intelligence for Humans
// Volume 1: Fundamental Algorithms
// C# Version
// http://www.aifh.org
// http://www.jeffheaton.com
//
// Code repository:
// https://github.com/jeffheaton/aifh
//
// Copyright 2013 by Jeff Heaton
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

namespace AIFH_Vol1.Core.Randomize
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
