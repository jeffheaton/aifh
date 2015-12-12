package com.heatonresearch.aifh.examples.search;

import java.util.Arrays;

public class ModelSearchResults implements Comparable<ModelSearchResults> {
    private final int epochs;
    private final double error;
    private final Object[] hyperParameters;

    public ModelSearchResults(int epochs, double error, Object[] hyperParameters) {
        this.epochs = epochs;
        this.error = error;
        this.hyperParameters = hyperParameters;
    }

    public int getEpochs() {
        return epochs;
    }

    public double getError() {
        return error;
    }

    @Override
    public String toString() {
        return Arrays.toString(this.hyperParameters) + " : error=" + this.error + ", epocs=" + this.epochs;
    }

    @Override
    public int compareTo(ModelSearchResults o) {
        return Double.compare(this.error,o.getError());
    }
}
