package com.heatonresearch.aifh.error;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 7/27/13
 * Time: 8:23 AM
 * To change this template use File | Settings | File Templates.
 */
public interface ErrorCalculation {

    /**
     * Called to update for each number that should be checked.
     *
     * @param actual The actual number.
     * @param ideal  The ideal number.
     */
    void updateError(final double[] actual, final double[] ideal, final double significance);

    /**
     * Update the error with single values.
     *
     * @param actual The actual value.
     * @param ideal  The ideal value.
     */
    void updateError(final double actual, final double ideal);

    /**
     * Calculate the error with MSE.
     *
     * @return The current error for the neural network.
     */
    double calculate();

    void clear();

    int getSetSize();
}
