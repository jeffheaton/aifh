package com.heatonresearch.aifh.general.fns;

/**
 * The Mexican Hat, or Ricker wavelet, Radial Basis Function.
 * <p/>
 * It is usually only referred to as the "Mexican hat" in the Americas, due to
 * cultural association with the "sombrero". In technical nomenclature this
 * function is known as the Ricker wavelet, where it is frequently employed to
 * model seismic data.
 * <p/>
 * http://en.wikipedia.org/wiki/Mexican_Hat_Function
 */
public class MexicanHatFunction extends AbstractRBF {

    /**
     * Construct the Mexican Hat RBF. Each RBF will require space equal to (dimensions + 1) in the params vector.
     *
     * @param theDimensions The number of dimensions.
     * @param theParams     A vector to hold the parameters.
     * @param theIndex      The index into the params vector.  You can store multiple RBF's in a vector.
     */
    public MexicanHatFunction(final int theDimensions, final double[] theParams, final int theIndex) {
        super(theDimensions, theParams, theIndex);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double evaluate(final double[] x) {
        // calculate the "norm", but don't take square root
        // don't square because we are just going to square it
        double norm = 0;
        for (int i = 0; i < getDimensions(); i++) {
            double center = this.getCenter(i);
            norm += Math.pow(x[i] - center, 2);
        }

        // calculate the value

        return (1 - norm) * Math.exp(-norm / 2);

    }
}
