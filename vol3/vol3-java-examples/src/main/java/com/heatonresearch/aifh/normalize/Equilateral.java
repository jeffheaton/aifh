/*
 * Artificial Intelligence for Humans
 * Volume 3: Deep Learning and Neural Networks
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh
 *
 * Copyright 2014-2015 by Jeff Heaton
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
package com.heatonresearch.aifh.normalize;

import com.heatonresearch.aifh.AIFHError;

import java.io.Serializable;

/**
 * Used to produce an array of activations to classify data into groups. This
 * class is provided the number of groups, as well as the range that the
 * activations should fall into.
 * Guiver, John P., and Klimasauskas, Casimir, C. (1991).
 * "Applying Neural Networks, Part IV: Improving Performance." PC AI, July/August
 */
public class Equilateral implements Serializable {

    /**
     * The minimum number of fields to use equilateral encoding.
     */
    public static final int MIN_EQ = 3;

    /**
     * The matrix of values that was generated.
     */
    private final double[][] matrix;

    /**
     * Construct an equilateral matrix.
     *
     * @param count The number of sets, these will be the rows in the matrix.
     * @param low   The high value for the outputs.
     * @param high  The low value for the outputs.
     */
    public Equilateral(final int count, final double low, final double high) {
        if (count < MIN_EQ) {
            throw new AIFHError("Must have at least three classes.");
        }
        this.matrix = equilat(count, low, high);
    }

    /**
     * Decode a set of activations and see which set it has the lowest Euclidean
     * distance from.
     *
     * @param activations The output from the neural network.
     * @return The set that these activations were closest too.
     */
    public final int decode(final double[] activations) {
        double minValue = Double.POSITIVE_INFINITY;
        int minSet = -1;

        for (int i = 0; i < this.matrix.length; i++) {
            final double dist = getDistance(activations, i);
            if (dist < minValue) {
                minValue = dist;
                minSet = i;
            }
        }
        return minSet;
    }

    /**
     * Get the activations for the specified set.
     *
     * @param set The set to determine the activations for.
     * @return The activations for the specified sets.
     */
    public final double[] encode(final int set) {
        if (set < 0 || set > this.matrix.length) {
            throw new AIFHError("Class out of range for equilateral: " + set);
        }
        return this.matrix[set];
    }

    /**
     * Called internally to generate the matrix.
     *
     * @param n    The number of sets to generate for.
     * @param low  The high end of the range of values to generate.
     * @param high The low end of the range of values to generate.
     * @return One row for each set, the columns are the activations for that
     *         set.
     */
    private double[][] equilat(final int n,
                               final double low, final double high) {
        double r, f;
        final double[][] result = new double[n][n - 1];

        result[0][0] = -1;
        result[1][0] = 1.0;

        for (int k = 2; k < n; k++) {
            // scale the matrix so far
            r = k;
            f = Math.sqrt(r * r - 1.0) / r;
            for (int i = 0; i < k; i++) {
                for (int j = 0; j < k - 1; j++) {
                    result[i][j] *= f;
                }
            }

            r = -1.0 / r;
            for (int i = 0; i < k; i++) {
                result[i][k - 1] = r;
            }

            for (int i = 0; i < k - 1; i++) {
                result[k][i] = 0.0;
            }
            result[k][k - 1] = 1.0;
        }

        // scale it
        for (int row = 0; row < result.length; row++) {
            for (int col = 0; col < result[0].length; col++) {
                final double min = -1;
                final double max = 1;
                result[row][col] = ((result[row][col] - min) / (max - min))
                        * (high - low) + low;
            }
        }

        return result;
    }

    /**
     * Get the Euclidean distance between the specified data and the set number.
     *
     * @param data The data to check.
     * @param set  The set to check.
     * @return The distance.
     */
    public final double getDistance(final double[] data, final int set) {
        double result = 0;
        for (int i = 0; i < data.length; i++) {
            result += Math.pow(data[i] - this.matrix[set][i], 2);
        }
        return Math.sqrt(result);
    }

}
