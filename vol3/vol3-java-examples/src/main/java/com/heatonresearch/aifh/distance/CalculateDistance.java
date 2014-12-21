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

package com.heatonresearch.aifh.distance;

/**
 * Calculate the distance between two vectors.  These vectors are double arrays.
 * Both vectors must be of the same length. These two vectors can be thought of
 * positions in a potentially high dimension space.
 * <p/>
 * You can think of a distance metric as measuring the similarity between two vectors.
 */
public interface CalculateDistance {
    /**
     * Calculate the distance between two vectors, both full length and starting at position 0.
     *
     * @param position1 The first vector.
     * @param position2 The second vector.
     * @return The distance.
     */
    double calculate(double[] position1, double[] position2);

    /**
     * Calculate the distance between two vectors.
     *
     * @param position1 The first vector.
     * @param pos1      The position to use in the first vector.
     * @param position2 The second vector.
     * @param pos2      The position to use in the second vector.
     * @param length    The length of both vectors, they must be the same.
     * @return The distance.
     */
    double calculate(double[] position1, int pos1, double[] position2, int pos2, int length);
}
