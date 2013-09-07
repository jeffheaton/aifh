package com.heatonresearch.aifh.randomize;

import java.security.SecureRandom;
import java.util.Random;

/**
 * A wrapper over Java's crypto secure random number generator.
 */
public class SecureGenerateRandom extends AbstractGenerateRandom {

    /**
     * The underlying random number generator.
     */
    private final Random rand;

    /**
     * Construct the random number generator.
     *
     * @param seed The seed.
     */
    public SecureGenerateRandom(long seed) {
        byte[] s = {(byte) seed};
        this.rand = new SecureRandom(s);
    }

    /**
     * Construct with a time-based seed.
     */
    public SecureGenerateRandom() {
        this.rand = new SecureRandom();
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
