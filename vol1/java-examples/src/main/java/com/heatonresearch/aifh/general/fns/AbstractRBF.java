package com.heatonresearch.aifh.general.fns;

import java.text.NumberFormat;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 7/27/13
 * Time: 5:16 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class AbstractRBF implements FnRBF {


    private double[] params;
    private int indexWidth;
    private int indexCenters;
    private int dimensions;


    public AbstractRBF(int theDimensions, double[] theParams, int theIndex) {
        this.dimensions = theDimensions;
        this.params = theParams;
        this.indexWidth = theIndex;
        this.indexCenters = theIndex + 1;
    }

    @Override
    public final double getCenter(final int dimension) {
        return this.params[indexCenters + dimension];
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public final int getDimensions() {
        return this.dimensions;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public final double getWidth() {
        return this.params[indexWidth];
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public final void setWidth(final double theWidth) {
        this.params[indexWidth] = theWidth;
    }

    public String toString() {
        final NumberFormat f = NumberFormat.getNumberInstance();
        f.setMinimumFractionDigits(2);

        StringBuilder result = new StringBuilder();
        result.append("[");
        result.append(this.getClass().getSimpleName());
        result.append(":width=");
        result.append(f.format(this.getWidth()));
        result.append(",center=");
        for (int i = 0; i < this.dimensions; i++) {
            if (i > 0) {
                result.append(",");
                result.append(f.format(this.params[this.indexCenters + i]));
            }
        }

        result.append("]");
        return result.toString();
    }

    @Override
    public void setCenter(final int dimension, final double value) {
        this.params[indexCenters + dimension] = value;
    }
}
