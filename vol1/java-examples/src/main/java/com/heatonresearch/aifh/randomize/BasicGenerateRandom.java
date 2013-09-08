package com.heatonresearch.aifh.randomize;

import java.util.Random;

/**
 * A wrapper over Java's built in random number generator.
 */
public class BasicGenerateRandom extends AbstractGenerateRandom {

    /**
     * The underlying random number generator.
     */
    private final Random rand;

    /**
     * Construct a random number generator with the specified seed.
     *
     * @param seed The seed.
     */
    public BasicGenerateRandom(long seed) {
        this.rand = new Random(seed);
    }

    /**
     * Construct a random number generator with a time-based seed.
     */
    public BasicGenerateRandom() {
        this.rand = new Random();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int nextInt() {
        return this.rand.nextInt();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double nextDouble() {
        return this.rand.nextDouble();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double nextFloat() {
        return this.rand.nextFloat();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public long nextLong() {
        return this.rand.nextLong();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean nextBoolean() {
        return this.rand.nextBoolean();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double nextGaussian() {
        return this.rand.nextGaussian();
    }
}
