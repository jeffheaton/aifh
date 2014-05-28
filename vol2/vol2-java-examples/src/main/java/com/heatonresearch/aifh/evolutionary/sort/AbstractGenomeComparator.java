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
package com.heatonresearch.aifh.evolutionary.sort;

import java.io.Serializable;

/**
 * Provides base functionality for comparing genomes. Specifically the ability
 * to add bonuses and penalties.
 */
public abstract class AbstractGenomeComparator implements GenomeComparator,
        Serializable {

    /**
     * The serial ID.
     */
    private static final long serialVersionUID = 1L;

    /**
     * {@inheritDoc}
     */
    @Override
    public double applyBonus(final double value, final double bonus) {
        final double amount = value * bonus;
        if (shouldMinimize()) {
            return value - amount;
        } else {
            return value + amount;
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double applyPenalty(final double value, final double bonus) {
        final double amount = value * bonus;
        if (!shouldMinimize()) {
            return value - amount;
        } else {
            return value + amount;
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isBetterThan(final double d1, final double d2) {
        if (shouldMinimize()) {
            return d1 < d2;
        } else {
            return d1 > d2;
        }
    }
}
