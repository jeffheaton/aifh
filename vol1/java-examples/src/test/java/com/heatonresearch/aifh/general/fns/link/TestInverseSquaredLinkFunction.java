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

package com.heatonresearch.aifh.general.fns.link;

import com.heatonresearch.aifh.AIFH;
import com.heatonresearch.aifh.AIFHError;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * Test the squared link function.
 */
public class TestInverseSquaredLinkFunction {
    @Test
    public void testEvaluate() {
        final InverseSquaredLinkFunction fn = new InverseSquaredLinkFunction();
        final double[] x = {2};
        final double y = fn.evaluate(x);
        assertEquals(-0.25, y, AIFH.DEFAULT_PRECISION);
    }

    @Test(expected = AIFHError.class)
    public void testException() {
        final InverseSquaredLinkFunction fn = new InverseSquaredLinkFunction();
        final double[] x = {1, 2};
        fn.evaluate(x);
    }
}
