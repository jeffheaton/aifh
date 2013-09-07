package com.heatonresearch.aifh.randomize;

/**
 * Provides a foundation for most random number generation.  This allows the nextDouble to generate
 * the other types.
 */
public abstract class AbstractGenerateRandom implements GenerateRandom {

    /**
     * {@inheritDoc}
     */
    @Override
    public int nextInt(final int low, final int high) {
        return (low + (int) (nextDouble() * ((high - low))));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double nextDouble(final double high) {
        return nextDouble(0, high);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double nextDouble(final double low, final double high) {
        return (low + (nextDouble() * ((high - low))));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int nextInt(int range) {
        return nextInt(0, range);
    }
}
