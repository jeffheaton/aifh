/*
 * Artificial Intelligence for Humans
 * Volume 1: Fundamental Algorithms
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh

 * Copyright 2013 by Jeff Heaton
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * For more information on Heaton Research copyrights, licenses
 * and trademarks visit:
 * http://www.heatonresearch.com/copyright
 */

package com.heatonresearch.aifh.randomize;

/**
 * The Mersenne twister is a pseudo random number generator developed in 1997 by Makoto Matsumoto and
 * Takuji Nishimura that is based on a matrix linear recurrence over a finite binary field F2.
 * <p/>
 * References:
 * <p/>
 * http://www.cs.gmu.edu/~sean/research/
 * <p/>
 * http://en.wikipedia.org/wiki/Mersenne_twister
 * <p/>
 * Makato Matsumoto and Takuji Nishimura, "Mersenne Twister: A 623-Dimensionally Equidistributed Uniform
 * Pseudo-Random Number Generator", ACM Transactions on Modeling and. Computer Simulation,
 * Vol. 8, No. 1, January 1998, pp 3--30.
 */
public class MersenneTwisterGenerateRandom extends AbstractBoxMuller {

    private static final int N = 624;
    private static final int M = 397;
    private static final int MATRIX_A = 0x9908b0df;
    private static final int UPPER_MASK = 0x80000000;
    private static final int LOWER_MASK = 0x7fffffff;
    private static final int TEMPERING_MASK_B = 0x9d2c5680;
    private static final int TEMPERING_MASK_C = 0xefc60000;

    private int stateVector[];
    private int mti;
    private int mag01[];

    public MersenneTwisterGenerateRandom() {
        this(System.currentTimeMillis());
    }

    public MersenneTwisterGenerateRandom(final long seed) {
        setSeed(seed);
    }

    public MersenneTwisterGenerateRandom(final int[] array) {
        setSeed(array);
    }

    public void setSeed(final long seed) {
        stateVector = new int[N];

        mag01 = new int[2];
        mag01[0] = 0x0;
        mag01[1] = MATRIX_A;

        stateVector[0] = (int) seed;
        for (mti = 1; mti < N; mti++) {
            stateVector[mti] =
                    (1812433253 * (stateVector[mti - 1] ^ (stateVector[mti - 1] >>> 30)) + mti);
        }
    }

    public void setSeed(final int[] array) {
        int i, j, k;
        setSeed(19650218);
        i = 1;
        j = 0;
        k = (N > array.length ? N : array.length);
        for (; k != 0; k--) {
            stateVector[i] = (stateVector[i] ^ ((stateVector[i - 1] ^ (stateVector[i - 1] >>> 30)) * 1664525)) + array[j] + j;
            i++;
            j++;
            if (i >= N) {
                stateVector[0] = stateVector[N - 1];
                i = 1;
            }
            if (j >= array.length) j = 0;
        }
        for (k = N - 1; k != 0; k--) {
            stateVector[i] = (stateVector[i] ^ ((stateVector[i - 1] ^ (stateVector[i - 1] >>> 30)) * 1566083941)) - i;
            i++;
            if (i >= N) {
                stateVector[0] = stateVector[N - 1];
                i = 1;
            }
        }
        stateVector[0] = 0x80000000;
    }

    protected int next(final int bits) {
        int y;

        if (mti >= N) {
            int kk;

            for (kk = 0; kk < N - M; kk++) {
                y = (stateVector[kk] & UPPER_MASK) | (stateVector[kk + 1] & LOWER_MASK);
                stateVector[kk] = stateVector[kk + M] ^ (y >>> 1) ^ mag01[y & 0x1];
            }
            for (; kk < N - 1; kk++) {
                y = (stateVector[kk] & UPPER_MASK) | (stateVector[kk + 1] & LOWER_MASK);
                stateVector[kk] = stateVector[kk + (M - N)] ^ (y >>> 1) ^ mag01[y & 0x1];
            }
            y = (stateVector[N - 1] & UPPER_MASK) | (stateVector[0] & LOWER_MASK);
            stateVector[N - 1] = stateVector[M - 1] ^ (y >>> 1) ^ mag01[y & 0x1];

            mti = 0;
        }

        y = stateVector[mti++];
        y ^= y >>> 11;
        y ^= (y << 7) & TEMPERING_MASK_B;
        y ^= (y << 15) & TEMPERING_MASK_C;
        y ^= (y >>> 18);

        return y >>> (32 - bits);
    }

    @Override
    public double nextDouble() {
        return (((long) next(26) << 27) + next(27))
                / (double) (1L << 53);
    }

    public long nextLong() {
        return ((long) next(32) << 32) + next(32);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean nextBoolean() {
        return nextDouble() > 0.5;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public float nextFloat() {
        return (float) nextDouble();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int nextInt() {
        return (int) nextLong();
    }
}
