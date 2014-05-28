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
    public BasicGenerateRandom(final long seed) {
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
    public float nextFloat() {
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
