// Artificial Intelligence for Humans
// Volume 3: Deep Learning and Neural Networks
// C# Version
// http://www.aifh.org
// http://www.jeffheaton.com
//
// Code repository:
// https://github.com/jeffheaton/aifh
//
// Copyright 2015 by Jeff Heaton
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

namespace AIFH_Vol3.Core.Randomize
{
    /// <summary>
    ///     Multiply with Carry (MWC) is a pseudo random number generator computer science, multiply-with-carry (MWC)
    ///     is a method invented by George Marsaglia for generating sequences of random integers based on an initial set
    ///     from two to many thousands of randomly chosen seed values. The main advantages of the MWC method are that it
    ///     invokes simple computer integer arithmetic and leads to very fast generation of sequences of random numbers
    ///     with immense periods.
    ///     This class was implemented using information from the following sources:
    ///     http://www.javaprogrammingforums.com/blogs/helloworld922/11-complimentary-multiply-carry-better-way-generate-pseudo-random-numbers.html
    ///     http://en.wikipedia.org/wiki/Multiply-with-carry
    /// </summary>
    public class MultiplyWithCarryGenerateRandom : AbstractBoxMuller
    {
        private readonly long[] _seed;
        private long _c;
        private long _multiplier;
        private int _n;
        private int _r;

        public MultiplyWithCarryGenerateRandom(long seed)
            : this(new[] {seed}, seed/2, 64, 987657110L)
        {
        }

        public MultiplyWithCarryGenerateRandom()
            : this(new long[] {Environment.TickCount}, Environment.TickCount*64, 64, 987657110L)
        {
        }

        public MultiplyWithCarryGenerateRandom(long[] seeds, long carry, int r, long multiplier)
        {
            SetR(r);
            SetMultiplier(multiplier);
            _seed = new long[r];
            if (seeds == null || seeds.Length == 0)
            {
                seeds = new long[] {Environment.TickCount};
            }

            var rnd = new LinearCongruentialRandom(seeds[0]);
            _c = (carry & 0xFFFFFFFFL)%multiplier;
            for (var i = 0; i < r; ++i)
            {
                if (i < seeds.Length)
                {
                    _seed[i] = seeds[i] & 0xFFFFFFFFL;
                }
                else
                {
                    _seed[i] = rnd.NextInt() & 0xFFFFFFFFL;
                }
                if (_seed[i] == 0xFFFFFFFFL)
                {
                    _seed[i] = 1L;
                }
            }
        }

        /**
         * {@inheritDoc}
         */

        public override double NextDouble()
        {
            return (((long) Next(26) << 27) + Next(27))
                   /(double) (1L << 53);
        }

        private int Next(int bits)
        {
            var t = _multiplier*_seed[_n] + _c;
            var d32 = t >> 32;
            _c = d32 + ((t & 0xFFFFFFFFL) >= 0xFFFFFFFFL - d32 ? 1L : 0L);
            _seed[_n] = 0xFFFFFFFEL - (t & 0xFFFFFFFFL) - (_c - d32 << 32) - _c & 0xFFFFFFFFL;
            var result = _seed[_n];
            _n = _n + 1 & _r - 1;
            return (int) (result >> 32 - bits);
        }

        private void SetMultiplier(long theMultiplier)
        {
            _multiplier = theMultiplier;
        }

        private void SetR(int theR)
        {
            if (theR <= 0)
            {
                theR = 256;
            }
            else
            {
                var validR = true;
                long a = theR;
                while (a != 1 && validR)
                {
                    if (a%2 != 0)
                    {
                        theR = 256;
                        validR = false;
                    }
                    a >>= 1;
                }
            }
            _r = theR;
        }

        /**
         * {@inheritDoc}
         */

        public override long NextLong()
        {
            return ((long) Next(32) << 32) + Next(32);
        }

        /**
         * {@inheritDoc}
         */

        public override bool NextBoolean()
        {
            return NextDouble() > 0.5;
        }

        /**
         * {@inheritDoc}
         */

        public override float NextFloat()
        {
            return (float) NextDouble();
        }

        /**
         * {@inheritDoc}
         */

        public override int NextInt()
        {
            return (int) NextLong();
        }
    }
}