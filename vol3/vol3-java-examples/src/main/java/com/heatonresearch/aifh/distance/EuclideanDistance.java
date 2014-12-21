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
 * The Euclidean distance is the straight-line distance between two points.  It is calculated by taking the
 * square root of the sum of squares differences between each point in the vector.
 * <p/>
 * http://www.heatonresearch.com/wiki/Euclidean_Distance
 */
public class EuclideanDistance extends AbstractDistance {

    /**
     * {@inheritDoc}
     */
    @Override
    public double calculate(final double[] position1, final int pos1, final double[] position2, final int pos2, final int length) {
        double sum = 0;
        for (int i = 0; i < length; i++) {
            final double d = position1[i + pos1] - position2[i + pos1];
            sum += d * d;
        }
        return Math.sqrt(sum);
    }
}
