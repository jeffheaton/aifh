package com.heatonresearch.aifh.examples.capstone.model.milestone1;

/**
 * Calculate the mean of a series of doubles.
 */
public class CalcMean {
    /**
     * How many values have we encountered so far.
     */
    private int count;

    /**
     * What is the sum of values.
     */
    private double sum;

    /**
     * Update mean for a new value.
     * @param d The next value.
     */
    public void update(double d) {
        this.sum+=d;
        this.count++;
    }

    /**
     * @return The calculated mean.
     */
    public double calculate() {
        return this.sum/this.count;
    }
}
