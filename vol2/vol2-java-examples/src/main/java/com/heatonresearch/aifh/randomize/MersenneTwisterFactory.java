package com.heatonresearch.aifh.randomize;

/**
 * A factory to create Mersenne Twister PRNG objects.
 */
public class MersenneTwisterFactory implements RandomFactory {
    /**
     * {@inheritDoc}
     */
    @Override
    public GenerateRandom factor() {
        return new MersenneTwisterGenerateRandom();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public RandomFactory factorFactory() {
        return new MersenneTwisterFactory();
    }
}
