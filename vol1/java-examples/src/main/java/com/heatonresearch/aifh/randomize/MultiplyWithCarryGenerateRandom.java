package com.heatonresearch.aifh.randomize;

/**
 * This class was implemented using information from the following sources:
 * <p/>
 * http://www.javaprogrammingforums.com/blogs/helloworld922/11-complimentary-multiply-carry-better-way-generate-pseudo-random-numbers.html
 * http://en.wikipedia.org/wiki/Multiply-with-carry
 */
public class MultiplyWithCarryGenerateRandom extends BasicGenerateRandom {
    private long c;
    private long multiplier;
    private int n = 0;
    private int r;
    private long[] seed;

    public MultiplyWithCarryGenerateRandom() {
        this(new long[]{System.currentTimeMillis()}, System.nanoTime(), 64, 987657110L);
    }

    public MultiplyWithCarryGenerateRandom(long[] seeds, long carry, int r, long multiplier) {
        setR(r);
        setMultiplier(multiplier);
        this.seed = new long[r];
        if (seeds == null || seeds.length == 0) {
            seeds = new long[]{System.currentTimeMillis()};
        }
        this.c = (carry & 0xFFFFFFFFL) % multiplier;
        for (int i = 0; i < r; ++i) {
            if (i < seeds.length) {
                this.seed[i] = seeds[i] & 0xFFFFFFFFL;
            } else {
                this.seed[i] = super.nextInt() & 0xFFFFFFFFL;
            }
            if (this.seed[i] == 0xFFFFFFFFL) {
                this.seed[i] = 1L;
            }
        }
    }

    @Override
    public double nextDouble() {
        return (((long) next(26) << 27) + next(27))
                / (double) (1L << 53);
    }

    private int next(int bits) {
        final long t = multiplier * seed[n] + c;
        final long d32 = t >>> 32;
        c = d32 + ((t & 0xFFFFFFFFL) >= 0xFFFFFFFFL - d32 ? 1L : 0L);
        seed[n] = 0xFFFFFFFEL - (t & 0xFFFFFFFFL) - (c - d32 << 32) - c & 0xFFFFFFFFL;
        final long result = seed[n];
        n = n + 1 & r - 1;
        return (int) (result >>> 32 - bits);
    }

    private void setMultiplier(long theMultiplier) {
        this.multiplier = theMultiplier;
    }

    private void setR(int theR) {
        if (theR <= 0) {
            theR = 256;
        } else {
            boolean validR = true;
            long a = theR;
            while (a != 1 && validR) {
                if (a % 2 != 0) {
                    theR = 256;
                    validR = false;
                }
                a >>>= 1;
            }
        }
        this.r = theR;
    }
}