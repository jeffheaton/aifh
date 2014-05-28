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
package com.heatonresearch.aifh.error;

/**
 * Calculates the error as the average of the sum of the squared differences between the actual and ideal vectors.
 * This is the most commonly used error calculation technique in this book.
 * <p/>
 * http://www.heatonresearch.com/wiki/Mean_Square_Error
 */
public class ErrorCalculationMSE extends AbstractErrorCalculation {

    /**
     * Calculate the error with MSE.
     *
     * @return The current error.
     */
    @Override
    public final double calculate() {
        if (this.setSize == 0) {
            return Double.POSITIVE_INFINITY;
        }
        return this.globalError / this.setSize;

    }

    /**
     * {@inheritDoc}
     */
    public ErrorCalculation create() {
        return new ErrorCalculationMSE();
    }
}
