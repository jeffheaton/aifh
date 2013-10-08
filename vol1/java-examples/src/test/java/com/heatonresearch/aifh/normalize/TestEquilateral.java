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

package com.heatonresearch.aifh.normalize;

import com.heatonresearch.aifh.AIFH;
import com.heatonresearch.aifh.AIFHError;
import com.heatonresearch.aifh.distance.CalculateDistance;
import com.heatonresearch.aifh.distance.EuclideanDistance;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

import static org.junit.Assert.assertEquals;

/**
 * Test equilateral.
 */
@RunWith(JUnit4.class)
public class TestEquilateral {

    @Test(expected = AIFHError.class)
    public void testTooFew() {
        new Equilateral(2, -1, 1);

    }

    @Test
    public void testEncode() {
        final Equilateral eq = new Equilateral(3, -1, 1);
        final double[] d = eq.encode(1);
        assertEquals(0.8660254037844386, d[0], AIFH.DEFAULT_PRECISION);
        assertEquals(-0.5, d[1], AIFH.DEFAULT_PRECISION);
    }

    @Test
    public void testDecode() {
        final Equilateral eq = new Equilateral(3, -1, 1);
        final double[] d0 = {0.866, 0.5};
        final double[] d1 = {-0.866, 0.5};
        final double[] d2 = {0, -1};
        assertEquals(2, eq.decode(d0));
        assertEquals(2, eq.decode(d1));
        assertEquals(0, eq.decode(d2));
    }

    @Test(expected = AIFHError.class)
    public void testError() {
        final Equilateral eq = new Equilateral(3, -1, 1);
        eq.encode(10);
    }

    /**
     * The idea of equalateral encoding is that every class is equal distant from the others.
     * This makes sure that is true.
     */
    @Test
    public void testAllEqual() {
        final Equilateral eq = new Equilateral(10, -1, 1);
        final CalculateDistance dc = new EuclideanDistance();
        double compareDist = 0;

        for (int x = 0; x < 10; x++) {
            double[] baseClass = eq.encode(x);
            for (int y = 0; y < 10; y++) {
                if (x != y) {
                    double[] otherClass = eq.encode(y);
                    double dist = dc.calculate(baseClass, otherClass);
                    if (compareDist < AIFH.DEFAULT_PRECISION) {
                        compareDist = dist;
                    } else {
                        assertEquals(compareDist, dist, AIFH.DEFAULT_PRECISION);
                    }
                }
            }
        }
    }
}
