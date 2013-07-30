package com.heatonresearch.aifh.error;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 7/27/13
 * Time: 8:36 AM
 * To change this template use File | Settings | File Templates.
 */
public class ErrorCalculationESS extends AbstractErrorCalculation {
    /**
     * Calculate the error with SSE.
     *
     * @return The current error for the neural network.
     */
    @Override
    public double calculate() {
        if (this.setSize == 0) {
            return Double.POSITIVE_INFINITY;
        }
        final double err = this.globalError / 2;
        return err;

    }
}
