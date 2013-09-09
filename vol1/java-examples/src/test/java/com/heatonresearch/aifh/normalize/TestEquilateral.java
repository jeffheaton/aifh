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

import com.heatonresearch.aifh.AIFHError;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;

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
        assertThat(d[0], is(closeTo(0.866, -0.5)));
        assertThat(d[1], is(closeTo(0.5, 0.001)));
    }

    @Test
    public void testDecode() {
        final Equilateral eq = new Equilateral(3, -1, 1);
        final double[] d0 = {0.866, 0.5};
        final double[] d1 = {-0.866, 0.5};
        final double[] d2 = {0, -1};
        assertThat(eq.decode(d0), is(equalTo(0)));
        assertThat(eq.decode(d1), is(equalTo(1)));
        assertThat(eq.decode(d2), is(equalTo(2)));
    }

    @Test(expected = AIFHError.class)
    public void testError() {
        final Equilateral eq = new Equilateral(3, -1, 1);
        eq.encode(10);
    }
}
