/*
 * Artificial Intelligence for Humans
 * Volume 2: Nature Inspired Algorithms
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh
 *
 * Copyright 2014 by Jeff Heaton
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
            final double center = getCenter(i);
            value += Math.pow(x[i] - center, 2) + (width * width);
        }
        return 1 / Math.sqrt(value);

    }
}
