package com.heatonresearch.aifh.general.fns;

/**
 * The Inverse Multiquadric Radial Basis Function.
 * <p/>
 * http://en.wikipedia.org/wiki/Radial_basis_function
 */
public class InverseMultiquadricFunction extends AbstractRBF {

    /**
     * Construct the Inverse Multiquadric RBF. Each RBF will require space equal to (dimensions + 1) in the
     * params vector.
     *
     * @param theDimensions The number of dimensions.
     * @param theParams     A vector to hold the parameters.
     * @param theIndex      The index into the params vector.  You can store multiple RBF's in a vector.
     */
    public InverseMultiquadricFunction(final int theDimensions, final double[] theParams, final int theIndex) {
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
            double center = getCenter(i);
            value += Math.pow(x[i] - center, 2) + (width * width);
        }
        return 1 / Math.sqrt(value);

    }
}
