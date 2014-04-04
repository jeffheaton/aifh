package com.heatonresearch.aifh.evolutionary;

/**
 * A genome is the basic blueprint for creating an phenome (organism) in Encog.
 * Some genomes also function as phenomes.
 *
 */
public interface Genome {
    /**
     * Copy from the specified genome into this one.
     *
     * @param source
     *            The source genome.
     */
    void copy(Genome source);

    /**
     * Get the adjusted score, this considers old-age penalties and youth
     * bonuses. If there are no such bonuses or penalties, this is the same as
     * the score.
     *
     * @return The adjusted score.
     */
    double getAdjustedScore();

    /**
     * @return The birth generation (or iteration).
     */
    int getBirthGeneration();

    /**
     * @return The population that this genome belongs to.
     */
    Population getPopulation();

    /**
     * @return The score for this genome.
     */
    double getScore();

    /**
     * Set the adjusted score.
     *
     * @param adjustedScore
     *            The adjusted score.
     */
    void setAdjustedScore(double adjustedScore);

    /**
     * Set the birth genertion (or iteration).
     *
     * @param birthGeneration
     *            The birth generation.
     */
    void setBirthGeneration(int birthGeneration);

    /**
     * Set the population that this genome belongs to.
     *
     * @param population
     *            The population that this genome belongs to.
     */
    void setPopulation(Population population);

    /**
     * Set the score.
     *
     * @param score
     *            The new score.
     */
    void setScore(double score);

    /**
     * @return Return the size of this genome. This size is a relative number
     *         that indicates the complexity of the genome.
     */
    int size();


    /**
     * @return The species for this genome.
     */
    Species getSpecies();

    /**
     * Set the species for this genome.
     * @param s The species.
     */
    void setSpecies(Species s);
}
