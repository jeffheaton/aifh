package com.heatonresearch.aifh.examples.error;

import com.heatonresearch.aifh.error.ErrorCalculation;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 7/27/13
 * Time: 3:58 PM
 * To change this template use File | Settings | File Templates.
 */
public class DataHolder {
    private double[][] actual;
    private double[][] ideal;

    public double[][] getActual() {
        return actual;
    }

    public void setActual(final double[][] actual) {
        this.actual = actual;
    }

    public double[][] getIdeal() {
        return ideal;
    }

    public void setIdeal(final double[][] ideal) {
        this.ideal = ideal;
    }

    public double calculateError(ErrorCalculation calc) {
        calc.clear();

        for (int row = 0; row < this.actual.length; row++) {
            calc.updateError(this.actual[row], this.ideal[row], 1.0);
        }

        return calc.calculate();
    }
}
