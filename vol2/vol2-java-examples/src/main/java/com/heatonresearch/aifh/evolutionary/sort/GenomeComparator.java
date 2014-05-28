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

import java.util.Comparator;

/**
 * Defines methods for comparing genomes. Also provides methods to apply bonuses
 * and penalties.
 */
public interface GenomeComparator extends Comparator<Genome> {
    /**
     * Apply a bonus, this is a simple percent that is applied in the direction
     * specified by the "should minimize" property of the score function.
     *
     * @param value The current value.
     * @param bonus The bonus.
     * @return The resulting value.
     */
    double applyBonus(double value, double bonus);

    /**
     * Apply a penalty, this is a simple percent that is applied in the
     * direction specified by the "should minimize" property of the score
     * function.
     *
     * @param value The current value.
     * @param bonus The penalty.
     * @return The resulting value.
     */
    double applyPenalty(double value, double bonus);

    /**
     * Determine if one score is better than the other.
     *
     * @param d1 The first score to compare.
     * @param d2 The second score to compare.
     * @return True if d1 is better than d2.
     */
    boolean isBetterThan(double d1, double d2);

    /**
     * Determine if one genome is better than the other genome.
     *
     * @param genome1 The first genome.
     * @param genome2 The second genome.
     * @return True, if genome1 is better than genome2.
     */
    boolean isBetterThan(Genome genome1, Genome genome2);

    /**
     * @return Returns true if the score should be minimized.
     */
    boolean shouldMinimize();

}
