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

package com.heatonresearch.aifh.error;

/**
 * An error calculation metric calculates the difference between two vector sets.  One vector set will be the ideal
 * expected output from a Machine Learning Algorithm.  The other vector set is the actual output.  Training a
 * Machine Learning Algorithm typically involves minimizing this error.
 * <p/>
 * Error calculation metrics are very similar to distance metrics.  However, an error calculation metric operates over
 * a set of vectors, whereas a distance metric operates over just two vectors.
 */
public interface ErrorCalculation {

    /**
     * Called to update for each number that should be checked.
     *
     * @param actual The actual number.
     * @param ideal  The ideal number.
     */
    void updateError(final double[] actual, final double[] ideal, final double significance);

    /**
     * Update the error with single values.
     *
     * @param actual The actual value.
     * @param ideal  The ideal value.
     */
    void updateError(final double actual, final double ideal);

    /**
     * Calculate the error with MSE.
     *
     * @return The current error for the neural network.
     */
    double calculate();

    /**
     * Clear the error calculation and start over.
     */
    void clear();

    /**
     * @return The total size of the set (vector size times number of vectors).
     */
    int getSetSize();
}
