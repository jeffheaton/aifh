package com.heatonresearch.aifh.regression;

import com.heatonresearch.aifh.general.fns.Fn;
import com.heatonresearch.aifh.general.fns.link.IdentityLinkFunction;
import com.heatonresearch.aifh.learning.RegressionAlgorithm;

public class MultipleLinearRegression implements RegressionAlgorithm {

    private double[] longTermMemory;
    private Fn linkFunction = new IdentityLinkFunction();

    public MultipleLinearRegression(int theInputCount) {
        this.longTermMemory = new double[theInputCount + 1];
    }

    @Override
    public double[] computeRegression(final double[] input) {
        double sum = 0;

        for (int i = 1; i < this.longTermMemory.length; i++) {
            sum += input[i - 1] * this.longTermMemory[i];
        }
        sum += this.longTermMemory[0];

        double[] result = new double[1];
        result[0] = sum;
        result[0] = this.linkFunction.evaluate(result);
        return result;
    }

    @Override
    public double[] getLongTermMemory() {
        return this.longTermMemory;
    }

    public Fn getLinkFunction() {
        return linkFunction;
    }

    public void setLinkFunction(final Fn linkFunction) {
        this.linkFunction = linkFunction;
    }
}