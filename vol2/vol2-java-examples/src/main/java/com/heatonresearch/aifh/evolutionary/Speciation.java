package com.heatonresearch.aifh.evolutionary;

import java.util.List;

/**
 * Defines a speciation strategy.
 */
public interface Speciation {

    /**
     * Setup the speciation strategy.
     * @param theOwner The owner.
     */
    void init(EvolutionaryAlgorithm theOwner);


    /**
     * Perform the speciation.
     * @param genomeList The genomes to speciate.
     */
    void performSpeciation(List<Genome> genomeList);
}

