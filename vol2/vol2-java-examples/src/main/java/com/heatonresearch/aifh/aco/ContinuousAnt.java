package com.heatonresearch.aifh.aco;

/**
 * An individual ant for continuous ACO.
 */
public class ContinuousAnt implements Comparable<ContinuousAnt> {

    /**
     * The score for this ant.
     */
    private double score;

    /**
     * The parameters for this ant.
     */
    private final double[] params;

    /**
     * True, if this ant should minimize.  This value should be the same for all ants.
     */
    private boolean shouldMinimize;

    /**
     * The constructor.
     * @param n The number of parameters (dimensions).
     * @param theShouldMinimize True, if we are minimizing.
     */
    public ContinuousAnt(int n, boolean theShouldMinimize) {
        this.params = new double[n];
        this.shouldMinimize = theShouldMinimize;
    }

    /**
     * @return The score for this ant.
     */
    public double getScore() {
        return score;
    }

    /**
     * Set the score for this ant.
     * @param score The score.
     */
    public void setScore(final double score) {
        this.score = score;
    }

    /**
     * @return The parameters for this ant.
     */
    public double[] getParams() {
        return params;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int compareTo(final ContinuousAnt o) {
        if( shouldMinimize ) {
            return Double.compare(getScore(),o.getScore());
        } else {
            return Double.compare(o.getScore(),getScore());
        }
    }
}
