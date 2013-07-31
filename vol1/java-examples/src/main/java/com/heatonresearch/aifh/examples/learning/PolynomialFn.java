package com.heatonresearch.aifh.examples.learning;

import com.heatonresearch.aifh.learning.RegressionAlgorithm;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 7/30/13
 * Time: 2:05 PM
 * To change this template use File | Settings | File Templates.
 */
public class PolynomialFn implements RegressionAlgorithm {

    private double[] longTermMemory;

    public PolynomialFn(int polyOrder) {
        this.longTermMemory = new double[polyOrder];
    }


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

    @Override
    public double[] getLongTermMemory() {
        return longTermMemory;
    }

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
