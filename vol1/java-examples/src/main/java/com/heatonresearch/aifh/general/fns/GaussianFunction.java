package com.heatonresearch.aifh.general.fns;

/**
 * The Gaussian function is a Radial Basis Function that describes the typical "bell curve", or "normal distribution".
 * <p/>
 * The Gaussian function requires paramaters that specify the width (over all dimensions), as well as the
 * centers of each dimension.  So a 3d Gaussian would have the parameters lined up as follows:
 * <p/>
 * params[0] = width (of all dimensions),
 * <p/>
 * params[1] = center of dimension 0,
 * <p/>
 * params[2] = center of dimension 1,
 * <p/>
 * params[3] = center of dimension 3
 * <p/>
 * http://en.wikipedia.org/wiki/Gaussian_function
 */
public class GaussianFunction extends AbstractRBF {


    /**
     * Construct the Gaussian RBF. Each RBF will require space equal to (dimensions + 1) in the params vector.
     *
     * @param theDimensions The number of dimensions.
     * @param theParams     A vector to hold the parameters.
     * @param theIndex      The index into the params vector.  You can store multiple RBF's in a vector.
     */
    public GaussianFunction(int theDimensions, double[] theParams, int theIndex) {
        super(theDimensions, theParams, theIndex);
    }

    /**
     * {@inheritDoc}
     */
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
