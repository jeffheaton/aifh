package com.heatonresearch.aifh.evolutionary;

import com.heatonresearch.aifh.randomize.GenerateRandom;

/**
 * Provides the interface to a selection operator. This allows genomes to be
 * selected for offspring production or elimination.
 */
public interface SelectionOperator {

    /**
     * Selects an fit genome.
     * @param rnd A random number generator.
     * @param species The species to select the genome from.
     * @return The selected genome.
     */
    int performSelection(GenerateRandom rnd, Species species);

    /**
     * Selects an unfit genome.
     * @param rnd A random number generator.
     * @param species The species to select the genome from.
     * @return The selected genome.
     */
    int performAntiSelection(GenerateRandom rnd, Species species);

    /**
     * @return The trainer being used.
     */
    EvolutionaryAlgorithm getTrainer();
}
