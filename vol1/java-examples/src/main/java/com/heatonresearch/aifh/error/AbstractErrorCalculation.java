package com.heatonresearch.aifh.error;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 7/27/13
 * Time: 8:31 AM
 * To change this template use File | Settings | File Templates.
 */
public abstract class AbstractErrorCalculation implements ErrorCalculation {

    /**
     * The overall error.
     */
    protected double globalError;

    /**
     * The size of a set.
     */
    protected int setSize;

    /**
     * {@inheritDoc}
     */
    @Override
    public void updateError(final double[] actual, final double[] ideal, final double significance) {
        for (int i = 0; i < actual.length; i++) {
            double delta = (ideal[i] - actual[i]) * significance;

            this.globalError += delta * delta;
        }

        this.setSize += ideal.length;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void updateError(final double actual, final double ideal) {

        double delta = ideal - actual;

        this.globalError += delta * delta;

        this.setSize++;

    }
}
