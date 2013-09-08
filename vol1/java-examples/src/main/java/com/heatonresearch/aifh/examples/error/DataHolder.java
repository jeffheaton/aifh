package com.heatonresearch.aifh.examples.error;

import com.heatonresearch.aifh.error.ErrorCalculation;

/**
 * Simple data holder for actual and ideal.  Just used for this example.
 */
public class DataHolder {

    /**
     * The actual data.
     */
    private double[][] actual;

    /**
     * The ideal data, what the actual should have been.
     */
    private double[][] ideal;

    /**
     * Set the actual data.
     *
     * @param actual The actual data.
     */
    public void setActual(final double[][] actual) {
        this.actual = actual;
    }

    /**
     * Set the ideal data.
     *
     * @param ideal The ideal data.
     */
    public void setIdeal(final double[][] ideal) {
        this.ideal = ideal;
    }

    /**
     * Calculate the error with the specified error calculation.
     *
     * @param calc The error calculation.
     * @return The error.
     */
    public double calculateError(ErrorCalculation calc) {
        calc.clear();

        for (int row = 0; row < this.actual.length; row++) {
            calc.updateError(this.actual[row], this.ideal[row], 1.0);
        }

        return calc.calculate();
    }
}
