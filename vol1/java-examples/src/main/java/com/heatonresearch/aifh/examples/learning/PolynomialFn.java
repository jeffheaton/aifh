package com.heatonresearch.aifh.examples.learning;

import com.heatonresearch.aifh.learning.RegressionAlgorithm;

/**
 * A simple polynomial function, of a specified order. Implemented as a regression algorithm.
 * <p/>
 * http://en.wikipedia.org/wiki/Polynomial
 */
public class PolynomialFn implements RegressionAlgorithm {
    /**
     * The coefficients.  The first is the intercept.
     */
    private final double[] longTermMemory;

    /**
     * Construct a polynomial function.
     *
     * @param polyOrder The order.
     */
    public PolynomialFn(int polyOrder) {
        this.longTermMemory = new double[polyOrder];
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double[] computeRegression(final double[] input) {
        double x = input[0];
        double y = 0;

        for (int i = 0; i < this.longTermMemory.length; i++) {
            y += this.longTermMemory[i] * Math.pow(x, (double) i);
        }

        double[] result = new double[1];
        result[0] = y;

        return result;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double[] getLongTermMemory() {
        return longTermMemory;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        StringBuilder result = new StringBuilder();

        for (int i = this.longTermMemory.length - 1; i >= 0; i--) {
            double c = this.longTermMemory[i];

            if (result.length() > 0) {
                if (c >= 0) {
                    result.append('+');
                }
            }

            result.append(c);

            if (i >= 2) {
                result.append("x^");
                result.append(i);
            } else if (i >= 1) {
                result.append("x");
            }
        }

        return result.toString();
    }
}
