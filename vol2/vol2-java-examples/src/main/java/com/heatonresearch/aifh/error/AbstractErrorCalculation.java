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
package com.heatonresearch.aifh.error;

/**
 * An abstract error calculation class that provides some basic functionality.
 */
public abstract class AbstractErrorCalculation implements ErrorCalculation {

    /**
     * The overall error.
     */
    protected double globalError;

    /**
     * The size of a set.
     */
    protected int setSize;

    /**
     * {@inheritDoc}
     */
    @Override
    public void updateError(final double[] actual, final double[] ideal, final double significance) {
        for (int i = 0; i < actual.length; i++) {
            final double delta = (ideal[i] - actual[i]) * significance;

            this.globalError += delta * delta;
        }

        this.setSize += ideal.length;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void updateError(final double actual, final double ideal) {

        final double delta = ideal - actual;

        this.globalError += delta * delta;

        this.setSize++;

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void clear() {
        this.globalError = this.setSize = 0;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getSetSize() {
        return this.setSize;
    }
}
