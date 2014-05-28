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
package com.heatonresearch.aifh.evolutionary.opp;

import com.heatonresearch.aifh.general.collections.ChooseObject;
import com.heatonresearch.aifh.general.collections.ObjectHolder;
import com.heatonresearch.aifh.randomize.GenerateRandom;

/**
 * This class holds a list of evolutionary operators. Each operator is given a
 * probability weight. Based on the number of parents available a random
 * selection of an operator can be made based on the probability given each of
 * the operators.
 */
public class OperationList extends ChooseObject<EvolutionaryOperator> {

    /**
     * The serial id.
     */
    private static final long serialVersionUID = 1L;

    /**
     * Determine the maximum number of offspring that might be produced by any
     * of the operators in this list.
     *
     * @return The maximum number of offspring.
     */
    public int maxOffspring() {
        int result = 0;
        for (final ObjectHolder<EvolutionaryOperator> holder : getList()) {
            result = Math.max(result, holder.getObj().offspringProduced());
        }
        return result;
    }

    /**
     * Determine the maximum number of parents required by any of the operators
     * in the list.
     *
     * @return The maximum number of parents.
     */
    public int maxParents() {
        int result = Integer.MIN_VALUE;
        for (final ObjectHolder<EvolutionaryOperator> holder : getList()) {
            result = Math.max(result, holder.getObj().parentsNeeded());
        }
        return result;
    }

    /**
     * Pick a operator based on the number of parents available.
     *
     * @param rnd        A random number generator.
     * @param maxParents The maximum number of parents available.
     * @return The operator that was selected.
     */
    public EvolutionaryOperator pickMaxParents(final GenerateRandom rnd,
                                               final int maxParents) {

        // determine the total probability of eligible operators
        double total = 0;
        for (final ObjectHolder<EvolutionaryOperator> holder : getList()) {
            if (holder.getObj().parentsNeeded() <= maxParents) {
                total += holder.getProbability();
            }
        }

        // choose an operator
        final double r = rnd.nextDouble() * total;
        double current = 0;
        for (final ObjectHolder<EvolutionaryOperator> holder : getList()) {
            if (holder.getObj().parentsNeeded() <= maxParents) {
                current += holder.getProbability();
                if (r < current) {
                    return holder.getObj();
                }
            }
        }

        return null;
    }

}
