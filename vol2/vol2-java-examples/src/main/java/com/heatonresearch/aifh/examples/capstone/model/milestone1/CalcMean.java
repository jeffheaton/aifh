package com.heatonresearch.aifh.examples.capstone.model.milestone1;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 5/16/14
 * Time: 6:55 AM
 * To change this template use File | Settings | File Templates.
 */
public class CalcMean {
    private int count;
    private double sum;

    public void update(double d) {
        this.sum+=d;
        this.count++;
    }

    public double calculate() {
        return this.sum/this.count;
    }
}
