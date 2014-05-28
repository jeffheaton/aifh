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

import com.heatonresearch.aifh.evolutionary.genome.Genome;

import java.io.Serializable;

/**
 * Use this comparator to maximize the adjusted score.
 */
public class MaximizeAdjustedScoreComp extends AbstractGenomeComparator
        implements Serializable {

    /**
     * The serial ID.
     */
    private static final long serialVersionUID = 1L;

    /**
     * {@inheritDoc}
     */
    @Override
    public int compare(final Genome p1, final Genome p2) {
        return Double.compare(p2.getAdjustedScore(), p1.getAdjustedScore());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isBetterThan(final Genome prg, final Genome betterThan) {
        return isBetterThan(prg.getAdjustedScore(),
                betterThan.getAdjustedScore());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean shouldMinimize() {
        return false;
    }
}
