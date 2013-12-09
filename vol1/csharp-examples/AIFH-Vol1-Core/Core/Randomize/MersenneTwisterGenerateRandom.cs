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
    /// The Mersenne twister is a pseudo random number generator developed in 1997 by Makoto Matsumoto and
    /// Takuji Nishimura that is based on a matrix linear recurrence over a finite binary field F2.
    ///
    /// References:
    ///
    /// http://www.cs.gmu.edu/~sean/research/
    /// http://en.wikipedia.org/wiki/Mersenne_twister
    /// 
    /// Makato Matsumoto and Takuji Nishimura, "Mersenne Twister: A 623-Dimensionally Equidistributed Uniform
    /// Pseudo-Random Number Generator", ACM Transactions on Modeling and. Computer Simulation,
    /// Vol. 8, No. 1, January 1998, pp 3--30.
    /// </summary>
    public class MersenneTwisterGenerateRandom : AbstractBoxMuller
    {
        private const Int32 N = 624;
        private const Int32 M = 397;
        private const UInt32 MatrixA = 0x9908b0df;
        private const UInt32 UpperMask = 0x80000000;
        private const UInt32 LowerMask = 0x7fffffff;
        private const UInt32 TemperingMaskB = 0x9d2c5680;
        private const UInt32 TemperingMaskC = 0xefc60000;

        private UInt32[] _stateVector;
        private UInt16 _mti;
        private UInt32[] _mag01;

        public MersenneTwisterGenerateRandom()
            : this((uint)Environment.TickCount)
        {
        }

        public MersenneTwisterGenerateRandom(UInt32 seed)
        {
            SetSeed(seed);
        }

        public MersenneTwisterGenerateRandom(UInt32[] array)
        {
            SetSeed(array);
        }

        public void SetSeed(UInt32 seed)
        {
            _stateVector = new UInt32[N];

            _mag01 = new UInt32[2];
            _mag01[0] = 0x0;
            _mag01[1] = MatrixA;

            _stateVector[0] = seed;
            for (_mti = 1; _mti < N; _mti++)
            {
                _stateVector[_mti] =
                        (1812433253 * (_stateVector[_mti - 1] ^ (_stateVector[_mti - 1] >> 30)) + _mti);
            }
        }

        public void SetSeed(UInt32[] array)
        {
            Int32 i, j, k;
            SetSeed(19650218);
            i = 1;
            j = 0;
            k = (N > array.Length ? N : array.Length);
            for (; k != 0; k--)
            {

                _stateVector[i] = (uint)((_stateVector[i] ^ ((_stateVector[i - 1] ^ (_stateVector[i - 1] >> 30)) * 1664525U)) + array[j] + j);
                i++;
                j++;
                if (i >= N)
                {
                    _stateVector[0] = _stateVector[N - 1];
                    i = 1;
                }
                if (j >= array.Length) j = 0;
            }
            for (k = N - 1; k != 0; k--)
            {
                _stateVector[i] = (uint)((_stateVector[i] ^ ((_stateVector[i - 1] ^ (_stateVector[i - 1] >> 30)) * 1566083941U)) - i);
                i++;
                if (i >= N)
                {
                    _stateVector[0] = _stateVector[N - 1];
                    i = 1;
                }
            }
            _stateVector[0] = 0x80000000;
        }

        protected UInt32 Next(int bits)
        {
            UInt32 y;

            if (_mti >= N)
            {
                Int16 kk;

                for (kk = 0; kk < N - M; kk++)
                {
                    y = (_stateVector[kk] & UpperMask) | (_stateVector[kk + 1] & LowerMask);
                    _stateVector[kk] = _stateVector[kk + M] ^ (y >> 1) ^ _mag01[y & 0x1];
                }
                for (; kk < N - 1; kk++)
                {
                    y = (_stateVector[kk] & UpperMask) | (_stateVector[kk + 1] & LowerMask);
                    _stateVector[kk] = _stateVector[kk + (M - N)] ^ (y >> 1) ^ _mag01[y & 0x1];
                }
                y = (_stateVector[N - 1] & UpperMask) | (_stateVector[0] & LowerMask);
                _stateVector[N - 1] = _stateVector[M - 1] ^ (y >> 1) ^ _mag01[y & 0x1];

                _mti = 0;
            }

            y = _stateVector[_mti++];
            y ^= y >> 11;
            y ^= (y << 7) & TemperingMaskB;
            y ^= (y << 15) & TemperingMaskC;
            y ^= (y >> 18);

            return y >> (32 - bits);
        }

        public override double NextDouble()
        {
            return (((long)Next(26) << 27) + Next(27))
                    / (double)(1L << 53);
        }

        public override long NextLong()
        {
            uint u1 = Next(32);
            uint u2 = Next(32);
            return ((long)u1 << 32) + u2;
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
