package com.heatonresearch.aifh.evolutionary;

import com.heatonresearch.aifh.learning.score.Scorable;

/**
 * A CODEC defines how to transfer between a genome and phenome. Every CODEC
 * should support genome to phenome. However, not every code can transform a
 * phenome into a genome.
 */
public interface GeneticCODEC {

    /**
     * Decode the specified genome into a phenome. A phenome is an actual
     * instance of a genome that you can query.
     *
     * @param genome
     *            The genome to decode.
     * @return The phenome.
     */
    Scorable decode(Genome genome);

    /**
     * Attempt to build a genome from a phenome. Note: not all CODEC's support
     * this. If it is unsupported, an exception will be thrown.
     *
     * @param phenotype
     *            The phenotype.
     * @return The genome.
     */
    Genome encode(Scorable phenotype);
}
