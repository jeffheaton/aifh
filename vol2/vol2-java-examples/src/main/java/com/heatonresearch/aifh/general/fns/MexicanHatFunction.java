/*
 * Artificial Intelligence for Humans
 * Volume 1: Fundamental Algorithms
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh

 * Copyright 2013 by Jeff Heaton
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * For more information on Heaton Research copyrights, licenses
 * and trademarks visit:
 * http://www.heatonresearch.com/copyright
 */

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
            final double center = this.getCenter(i);
            norm += Math.pow(x[i] - center, 2);
        }

        // calculate the value

        return (1 - norm) * Math.exp(-norm / 2);

    }
}
