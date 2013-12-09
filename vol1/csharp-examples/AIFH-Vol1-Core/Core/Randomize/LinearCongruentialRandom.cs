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
    /// A Linear Congruential random number generator.  A Linear Congruential Generator (LCG) yields a sequence of
    /// randomized numbers calculated with a linear equation. The method represents one of the oldest and best-known
    /// pseudorandom number generator algorithms. Most programming languages use this technique.
    ///
    /// http://en.wikipedia.org/wiki/Linear_congruential_generator
    /// Donald Knuth, The Art of Computer Programming, Volume 3, Section 3.2.1
    /// </summary>
    public class LinearCongruentialRandom : AbstractBoxMuller
    {
        /// <summary>
        /// First part of default mod.
        /// </summary>
        public const long DefaultMod1 = 2L;

        /// <summary>
        /// Second part of default mod.
        /// </summary>
        public const long DefaultMod2 = 32L;

        /// <summary>
        /// Default mult.
        /// </summary>
        public static long DefaultMult = 1103515245L;

        /// <summary>
        /// Default inc.
        /// </summary>
        public static long DefaultInc = 12345L;

        /// <summary>
        /// The modulus.
        /// </summary>
        private readonly long _modulus;

        /// <summary>
        /// The multiplier.
        /// </summary>
        private readonly long _multiplier;

        /// <summary>
        /// The amount to increment.
        /// </summary>
        private readonly long _increment;

        /// <summary>
        /// The current seed, set to an initial value and always holds the value of
        /// the last random number generated.
        /// </summary>
        private long _seed;

        /// <summary>
        /// The maximum rand number that the standard GCC based LCG will generate.
        /// </summary>
        public const long MaxRand = 4294967295L;

        /// <summary>
        /// Construct the default LCG. You need only specify a seed. 
        /// </summary>
        /// <param name="theSeed">The seed to use.</param>
        public LinearCongruentialRandom(long theSeed)
            : this((long)Math.Pow(DefaultMod1, DefaultMod2),
                DefaultMult, DefaultInc, theSeed)
        {
        }

        /// <summary>
        /// Constructor to use a seed equal to system time.
        /// </summary>
        public LinearCongruentialRandom()
            : this(Environment.TickCount)
        {
        }

        /// <summary>
        /// Create a LCG with the specified modulus, multiplier and increment. Unless
        /// you REALLY KNOW WHAT YOU ARE DOING, just use the constructor that just
        /// takes a seed. It will set these values to the same as set by the GCC C
        /// compiler. Setting these values wrong can create fairly useless random
        /// numbers.
        /// </summary>
        /// <param name="theModulus">The modulus for the LCG algorithm.</param>
        /// <param name="theMultiplier">The multiplier for the LCG algorithm.</param>
        /// <param name="theIncrement">The increment for the LCG algorithm.</param>
        /// <param name="theSeed">The seed for the LCG algorithm. Using the same seed will give
        /// the same random number sequence each time, whether in Java or DotNet.</param>
        public LinearCongruentialRandom(long theModulus,
                                        long theMultiplier, long theIncrement,
                                        long theSeed)
        {
            _modulus = theModulus;
            _multiplier = theMultiplier;
            _increment = theIncrement;
            _seed = theSeed % MaxRand;
        }

        /// <summary>
        /// The LCG increment.
        /// </summary>
        public long Increment
        {
            get
            {
                return _increment;
            }
        }

        /// <summary>
        /// The LCG modulus.
        /// </summary>
        public long Modulus
        {
            get
            {
                return _modulus;
            }
        }

        /// <summary>
        /// The LCG multiplier.
        /// </summary>
        public long Multiplier
        {
            get
            {
                return _multiplier;
            }
        }

        /// <summary>
        /// The current seed. Set to a constant to start, thereafter the
        /// previously generated random number.
        /// </summary>
        public long Seed
        {
            get
            {
                return _seed;
            }
        }

        /// <summary>
        /// The next random number as a double between 0 and 1.
        /// </summary>
        /// <returns>The next random number as a double between 0 and 1.</returns>
        public override double NextDouble()
        {
            return (double)NextLong() / MaxRand;
        }

        /// <summary>
        /// The next random number as a long between 0 and MAX_RAND.
        /// </summary>
        /// <returns>The next random number as a long between 0 and MAX_RAND.</returns>
        public override long NextLong()
        {
            _seed = (_multiplier * _seed + _increment)
                    % _modulus;
            return _seed;
        }

        /// <inheritdoc/>
        public override bool NextBoolean()
        {
            return NextDouble() > 0.5;
        }

        /// <inheritdoc/>
        public override float NextFloat()
        {
            return (float)NextDouble();
        }


        /// <inheritdoc/>
        public override int NextInt()
        {
            return (int)NextLong();
        }
    }
}
