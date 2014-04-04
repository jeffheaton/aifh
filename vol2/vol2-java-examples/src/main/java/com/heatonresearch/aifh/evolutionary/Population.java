package com.heatonresearch.aifh.evolutionary;

import java.util.List;

/**
 * Defines a population of genomes.
 */
public interface Population {
    /**
     * Clear all genomes from this population.
     */
    void clear();

    /**
     * Create a species.
     *
     * @return The newly created species.
     */
    Species createSpecies();

    /**
     * Determine which species has the top genome.
     *
     * @return The species with the top genome.
     */
    Species determineBestSpecies();

    /**
     * Flatten the species into a single list of genomes.
     *
     * @return The genomes that make up all species in the population.
     */
    List<Genome> flatten();

    /**
     * @return The best genome in the population.
     */
    Genome getBestGenome();

    /**
     * @return The max size that an individual can become.
     */
    int getMaxIndividualSize();

    /**
     * @return The max population size.
     */
    int getPopulationSize();

    /**
     * @return The species that make up the population.
     */
    List<Species> getSpecies();

    /**
     * Set the best genome.
     *
     * @param bestGenome
     *            The best genome.
     */
    void setBestGenome(Genome bestGenome);

    /**
     * Set the max population size.
     *
     * @param populationSize
     *            The max population size.
     */
    void setPopulationSize(final int populationSize);

    /**
     * @return The size of the population.
     */
    int size();

    /**
     * Purge any invalid genomes.
     */
    void purgeInvalidGenomes();
}
