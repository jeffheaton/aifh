package com.heatonresearch.aifh.aco;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 5/6/14
 * Time: 1:32 AM
 * To change this template use File | Settings | File Templates.
 */
public class ContinuousAnt implements Comparable<ContinuousAnt> {
    private double score;
    private final double[] params;
    private boolean shouldMinimize;

    public ContinuousAnt(int n, boolean theShouldMinimize) {
        this.params = new double[n];
        this.shouldMinimize = theShouldMinimize;
    }

    public double getScore() {
        return score;
    }

    public void setScore(final double score) {
        this.score = score;
    }

    public double[] getParams() {
        return params;
    }

    @Override
    public int compareTo(final ContinuousAnt o) {
        if( shouldMinimize ) {
            return Double.compare(getScore(),o.getScore());
        } else {
            return Double.compare(o.getScore(),getScore());
        }
    }
}
