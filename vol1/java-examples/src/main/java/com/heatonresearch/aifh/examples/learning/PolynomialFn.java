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

package com.heatonresearch.aifh.examples.learning;

import com.heatonresearch.aifh.learning.RegressionAlgorithm;

/**
 * A simple polynomial function, of a specified order. Implemented as a regression algorithm.
 * <p/>
 * http://en.wikipedia.org/wiki/Polynomial
 */
public class PolynomialFn implements RegressionAlgorithm {
    /**
     * The coefficients.  The first is the intercept.
     */
    private final double[] longTermMemory;

    /**
     * Construct a polynomial function.
     *
     * @param polyOrder The order.
     */
    public PolynomialFn(final int polyOrder) {
        this.longTermMemory = new double[polyOrder];
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double[] computeRegression(final double[] input) {
        final double x = input[0];
        double y = 0;

        for (int i = 0; i < this.longTermMemory.length; i++) {
            y += this.longTermMemory[i] * Math.pow(x, (double) i);
        }

        final double[] result = new double[1];
        result[0] = y;

        return result;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double[] getLongTermMemory() {
        return longTermMemory;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        final StringBuilder result = new StringBuilder();

        for (int i = this.longTermMemory.length - 1; i >= 0; i--) {
            final double c = this.longTermMemory[i];

            if (result.length() > 0) {
                if (c >= 0) {
                    result.append('+');
                }
            }

            result.append(c);

            if (i >= 2) {
                result.append("x^");
                result.append(i);
            } else if (i >= 1) {
                result.append("x");
            }
        }

        return result.toString();
    }
}
