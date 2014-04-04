package com.heatonresearch.aifh.evolutionary;

import com.heatonresearch.aifh.randomize.GenerateRandom;

/**
 * An evolutionary operator is used to create new offspring genomes based on
 * parent genomes. There are a variety of means by which this can be done. The
 * number of parents required, as well as the number of offspring produced are
 * dependent on the operator. This interface defines key characteristics that
 * all operators must share.
 *
 * Most operators do not modify the parents. However, some mutation operators do
 * require that the children and parent array be the same. If the children and
 * parent arrays are the same, then the parent will be mutated.
 */
public interface EvolutionaryOperator {
    /**
     * Called to setup the evolutionary operator.
     *
     * @param theOwner
     *            The evolutionary algorithm used with this operator.
     */
    void init(EvolutionaryAlgorithm theOwner);

    /**
     * @return The number of offspring produced by this type of crossover.
     */
    int offspringProduced();

    /**
     * @return The number of parents needed.
     */
    int parentsNeeded();

    /**
     * Perform the evolutionary operation.
     *
     * @param rnd
     *            A random number generator.
     * @param parents
     *            The parents.
     * @param parentIndex
     *            The index into the parents array.
     * @param offspring
     *            The offspring.
     * @param offspringIndex
     *            An index into the offspring array.
     */
    void performOperation(GenerateRandom rnd, Genome[] parents, int parentIndex,
                          Genome[] offspring, int offspringIndex);
}
