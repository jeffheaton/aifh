package com.heatonresearch.aifh.general.fns;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 7/27/13
 * Time: 5:08 PM
 * To change this template use File | Settings | File Templates.
 */
public class GaussianFunction extends AbstractRBF {

    public GaussianFunction(int theDimensions, double[] theParams, int theIndex) {
        super(theDimensions, theParams, theIndex);
    }

    @Override
    public double evaluate(final double[] x) {
        double value = 0;
        final double width = getWidth();

        for (int i = 0; i < getDimensions(); i++) {
            double center = this.getCenter(i);
            value += Math.pow(x[i] - center, 2) / (2.0 * width * width);
        }
        return Math.exp(-value);
    }
}
