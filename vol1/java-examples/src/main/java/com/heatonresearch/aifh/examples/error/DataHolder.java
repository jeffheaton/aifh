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

package com.heatonresearch.aifh.examples.error;

import com.heatonresearch.aifh.error.ErrorCalculation;

/**
 * Simple data holder for actual and ideal.  Just used for this example.
 */
public class DataHolder {

    /**
     * The actual data.
     */
    private double[][] actual;

    /**
     * The ideal data, what the actual should have been.
     */
    private double[][] ideal;

    /**
     * Set the actual data.
     *
     * @param actual The actual data.
     */
    public void setActual(final double[][] actual) {
        this.actual = actual;
    }

    /**
     * Set the ideal data.
     *
     * @param ideal The ideal data.
     */
    public void setIdeal(final double[][] ideal) {
        this.ideal = ideal;
    }

    /**
     * Calculate the error with the specified error calculation.
     *
     * @param calc The error calculation.
     * @return The error.
     */
    public double calculateError(final ErrorCalculation calc) {
        calc.clear();

        for (int row = 0; row < this.actual.length; row++) {
            calc.updateError(this.actual[row], this.ideal[row], 1.0);
        }

        return calc.calculate();
    }
}
