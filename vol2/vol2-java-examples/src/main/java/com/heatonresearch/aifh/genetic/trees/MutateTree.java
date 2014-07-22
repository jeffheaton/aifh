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
package com.heatonresearch.aifh.genetic.trees;

import com.heatonresearch.aifh.evolutionary.genome.Genome;
import com.heatonresearch.aifh.evolutionary.opp.EvolutionaryOperator;
import com.heatonresearch.aifh.evolutionary.train.EvolutionaryAlgorithm;
import com.heatonresearch.aifh.randomize.GenerateRandom;

/**
 * Create a child tree as a mutation of the parent.  Do not modify the parent.
 */
public class MutateTree implements EvolutionaryOperator {

    /**
     * The owner.
     */
    private EvolutionaryAlgorithm owner;

    /**
     * The maximum length of a branch to graft.
     */
    private int maxGraftLength;

    /**
     * Construct the tree mutation object.
     * @param theMaxGraftLength The maximum graft length.
     */
    public MutateTree(int theMaxGraftLength) {
        this.maxGraftLength = Math.max(1, theMaxGraftLength);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void init(final EvolutionaryAlgorithm theOwner) {
        this.owner = theOwner;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int offspringProduced() {
        return 1;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int parentsNeeded() {
        return 1;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void performOperation(final GenerateRandom rnd, final Genome[] parents, final int parentIndex, final Genome[] offspring, final int offspringIndex) {
        TreeGenome parent1 = (TreeGenome) parents[parentIndex];
        EvaluateTree eval = parent1.getEvaluator();
        TreeGenome off1 = (TreeGenome) this.owner.getPopulation().getGenomeFactory().factor(parent1);
        RandomNodeResult off1Point = eval.sampleRandomNode(rnd, off1.getRoot());

        int len = rnd.nextInt(1, this.maxGraftLength + 1);
        TreeGenomeNode randomSequence = eval.grow(rnd, len);

        if (off1Point.getParent() == null) {
            off1.setRoot(randomSequence);
        } else {
            int idx = off1Point.getParent().getChildren().indexOf(off1Point.getChild());
            off1Point.getParent().getChildren().set(idx, randomSequence);
        }

        offspring[0] = off1;
    }
}
