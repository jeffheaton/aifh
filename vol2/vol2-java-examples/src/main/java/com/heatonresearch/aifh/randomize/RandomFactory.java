package com.heatonresearch.aifh.randomize;

/**
 * A factory to create random number generators of a specific type.
 */
public interface RandomFactory {
    GenerateRandom factor();
    RandomFactory factorFactory();
}
