/*
 * Artificial Intelligence for Humans
 * Volume 2: Nature Inspired Algorithms
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh
 *
 * Copyright 2014 by Jeff Heaton
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
 * A Linear Congruential random number generator.  A Linear Congruential Generator (LCG) yields a sequence of
 * randomized numbers calculated with a linear equation. The method represents one of the oldest and best-known
 * pseudorandom number generator algorithms. Most programming languages use this technique.
 * <p/>
 * http://en.wikipedia.org/wiki/Linear_congruential_generator
 * Donald Knuth, The Art of Computer Programming, Volume 3, Section 3.2.1
 */
public class LinearCongruentialRandom extends AbstractBoxMuller {

    /**
     * First part of default mod.
     */
    public static final long DEFAULT_MOD1 = 2L;

    /**
     * Second part of default mod.
     */
    public static final long DEFAULT_MOD2 = 32L;

    /**
     * Default mult.
     */
    public static final long DEFAULT_MULT = 1103515245L;

    /**
     * Default inc.
     */
    public static final long DEFAULT_INC = 12345L;

    /**
     * The modulus.
     */
    private final long modulus;

    /**
     * The multiplier.
     */
    private final long multiplier;

    /**
     * The amount to increment.
     */
    private final long increment;

    /**
     * The current seed, set to an initial value and always holds the value of
     * the last random number generated.
     */
    private long seed;

    /**
     * The maximum rand number that the standard GCC based LCG will generate.
     */
    public static final long MAX_RAND = 4294967295L;

    /**
     * Construct the default LCG. You need only specify a seed.
     *
     * @param theSeed The seed to use.
     */
    public LinearCongruentialRandom(final long theSeed) {
        this((long) Math.pow(DEFAULT_MOD1, DEFAULT_MOD2),
                DEFAULT_MULT, DEFAULT_INC, theSeed);
    }

    /**
     * Constructor to use a seed equal to system time.
     */
    public LinearCongruentialRandom() {
        this(System.currentTimeMillis());
    }

    /**
     * Create a LCG with the specified modulus, multiplier and increment. Unless
     * you REALLY KNOW WHAT YOU ARE DOING, just use the constructor that just
     * takes a seed. It will set these values to the same as set by the GCC C
     * compiler. Setting these values wrong can create fairly useless random
     * numbers.
     *
     * @param theModulus    The modulus for the LCG algorithm.
     * @param theMultiplier The multiplier for the LCG algorithm.
     * @param theIncrement  The increment for the LCG algorithm.
     * @param theSeed       The seed for the LCG algorithm. Using the same seed will give
     *                      the same random number sequence each time, whether in Java or
     *                      DotNet.
     */
    public LinearCongruentialRandom(final long theModulus,
                                    final long theMultiplier, final long theIncrement,
                                    final long theSeed) {
        super();
        this.modulus = theModulus;
        this.multiplier = theMultiplier;
        this.increment = theIncrement;
        this.seed = theSeed % MAX_RAND;
    }

    /**
     * @return The LCG increment.
     */
    public final long getIncrement() {
        return this.increment;
    }

    /**
     * @return The LCG modulus.
     */
    public final long getModulus() {
        return this.modulus;
    }

    /**
     * @return The LCG multiplier.
     */
    public final long getMultiplier() {
        return this.multiplier;
    }

    /**
     * @return The current seed. Set to a constant to start, thereafter the
     *         previously generated random number.
     */
    public final long getSeed() {
        return this.seed;
    }

    /**
     * @return The next random number as a double between 0 and 1.
     */
    @Override
    public final double nextDouble() {
        return (double) nextLong() / LinearCongruentialRandom.MAX_RAND;
    }

    /**
     * @return The next random number as a long between 0 and MAX_RAND.
     */
    @Override
    public final long nextLong() {
        this.seed = (this.multiplier * this.seed + this.increment)
                % this.modulus;
        return this.seed;
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
