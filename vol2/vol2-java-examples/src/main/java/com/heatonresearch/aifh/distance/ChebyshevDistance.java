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
package com.heatonresearch.aifh.distance;

/**
 * Chebyshev distance is the maximum absolute difference between any two vector elements.  This can be thought
 * of as the number of spaces that a king chess piece must travel between two squares in a 2D dimension space.
 * <p/>
 * http://www.heatonresearch.com/wiki/Chebyshev_Distance
 */
public class ChebyshevDistance extends AbstractDistance {

    /**
     * {@inheritDoc}
     */
    @Override
    public double calculate(final double[] position1, final int pos1, final double[] position2, final int pos2, final int length) {
        double result = 0;
        for (int i = 0; i < length; i++) {
            final double d = Math.abs(position1[pos1 + i] - position2[pos2 + i]);
            result = Math.max(d, result);
        }
        return result;
    }
}
