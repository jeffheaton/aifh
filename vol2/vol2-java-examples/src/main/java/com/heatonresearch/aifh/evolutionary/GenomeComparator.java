package com.heatonresearch.aifh.evolutionary;

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
     * @param value
     *            The current value.
     * @param bonus
     *            The bonus.
     * @return The resulting value.
     */
    double applyBonus(double value, double bonus);

    /**
     * Apply a penalty, this is a simple percent that is applied in the
     * direction specified by the "should minimize" property of the score
     * function.
     *
     * @param value
     *            The current value.
     * @param bonus
     *            The penalty.
     * @return The resulting value.
     */
    double applyPenalty(double value, double bonus);

    /**
     * Determine if one score is better than the other.
     *
     * @param d1
     *            The first score to compare.
     * @param d2
     *            The second score to compare.
     * @return True if d1 is better than d2.
     */
    boolean isBetterThan(double d1, double d2);

    /**
     * Determine if one genome is better than the other genome.
     *
     * @param genome1
     *            The first genome.
     * @param genome2
     *            The second genome.
     * @return True, if genome1 is better than genome2.
     */
    boolean isBetterThan(Genome genome1, Genome genome2);

    /**
     * @return Returns true if the score should be minimized.
     */
    boolean shouldMinimize();

}

