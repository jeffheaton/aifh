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

import com.heatonresearch.aifh.evolutionary.genome.Genome;
import com.heatonresearch.aifh.evolutionary.train.EvolutionaryAlgorithm;
import com.heatonresearch.aifh.general.collections.ObjectHolder;
import com.heatonresearch.aifh.randomize.GenerateRandom;

/**
 * A compound operator randomly chooses sub-operators to perform the actual
 * operation. Each of the sub-operators can be provided with a weighting.
 */
public class CompoundOperator implements EvolutionaryOperator {

    /**
     * The owner of this operator.
     */
    private EvolutionaryAlgorithm owner;

    /**
     * The sub-operators that make up this compound operator.
     */
    private final OperationList components = new OperationList();

    /**
     * @return the components
     */
    public OperationList getComponents() {
        return this.components;
    }

    /**
     * @return the owner
     */
    public EvolutionaryAlgorithm getOwner() {
        return this.owner;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void init(final EvolutionaryAlgorithm theOwner) {
        this.owner = theOwner;
        for (final ObjectHolder<EvolutionaryOperator> obj : this.components
                .getList()) {
            obj.getObj().init(theOwner);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int offspringProduced() {
        return this.components.maxOffspring();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int parentsNeeded() {
        return this.components.maxOffspring();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void performOperation(final GenerateRandom rnd, final Genome[] parents,
                                 final int parentIndex, final Genome[] offspring,
                                 final int offspringIndex) {
        final EvolutionaryOperator opp = this.components.pick(rnd);
        opp.performOperation(rnd, parents, parentIndex, offspring,
                offspringIndex);
    }
}
