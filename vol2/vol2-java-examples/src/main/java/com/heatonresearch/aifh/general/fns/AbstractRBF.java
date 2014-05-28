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

import java.text.NumberFormat;

/**
 * Provides the basics for an RBF function.  RBF functions take their "parameters" from a vector (and starting index).
 * This allows many RBF's to be "stacked" together in a single vector.  RBF parameters are: a single width and a
 * vector of centers.  Therefor the size required to store one RBF is (dimensions + 1).  There is no peak parameter,
 * the peak is assumed to be 1.
 */
public abstract class AbstractRBF implements FnRBF {

    /**
     * The parameter vector.  Holds the RBF width and centers.  This vector may hold multiple RBF's.
     */
    private final double[] params;

    /**
     * The index to the widths.
     */
    private final int indexWidth;

    /**
     * The index to the centers.
     */
    private final int indexCenters;

    /**
     * The dimensions.
     */
    private final int dimensions;

    /**
     * Construct the RBF. Each RBF will require space equal to (dimensions + 1) in the params vector.
     *
     * @param theDimensions The number of dimensions.
     * @param theParams     A vector to hold the paramaters.
     * @param theIndex      The index into the params vector.  You can store multiple RBF's in a vector.
     */
    public AbstractRBF(final int theDimensions, final double[] theParams, final int theIndex) {
        this.dimensions = theDimensions;
        this.params = theParams;
        this.indexWidth = theIndex;
        this.indexCenters = theIndex + 1;
    }

    /**
     * {@inheritDoc}
     */
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

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        final NumberFormat f = NumberFormat.getNumberInstance();
        f.setMinimumFractionDigits(2);

        final StringBuilder result = new StringBuilder();
        result.append("[");
        result.append(this.getClass().getSimpleName());
        result.append(":width=");
        result.append(f.format(this.getWidth()));
        result.append(",center=");
        for (int i = 0; i < this.dimensions; i++) {
            if (i > 0) {
                result.append(",");
            }
            result.append(f.format(this.params[this.indexCenters + i]));

        }

        result.append("]");
        return result.toString();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setCenter(final int dimension, final double value) {
        this.params[indexCenters + dimension] = value;
    }
}
