package com.heatonresearch.aifh.general.fns;

/**
 * A function that implements a radial basis function (RBF).
 */
public interface FnRBF extends Fn {

    /**
     * Get the center for the specified dimension.
     *
     * @param dimension The dimension.
     * @return The center.
     */
    double getCenter(int dimension);

    /**
     * Set the center for the specified dimension.
     *
     * @param dimension The dimension.
     * @param value     The value to set the center.
     */
    void setCenter(int dimension, double value);

    /**
     * @return The dimension count.
     */
    int getDimensions();

    /**
     * @return The width.
     */
    double getWidth();

    /**
     * Set the width.
     *
     * @param theWidth The width.
     */
    void setWidth(double theWidth);
}
