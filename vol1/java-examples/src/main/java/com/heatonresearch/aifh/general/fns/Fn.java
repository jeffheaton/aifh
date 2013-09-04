package com.heatonresearch.aifh.general.fns;

/**
 * A function.  Returns a single scalar variable, accepts a vector of x.
 */
public interface Fn {
    /**
     * Evaluate the function.
     *
     * @param x A vector input.
     * @return The output from the function.
     */
    double evaluate(double[] x);
}
