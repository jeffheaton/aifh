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

package com.heatonresearch.aifh.regression;

import com.heatonresearch.aifh.AIFH;
import com.heatonresearch.aifh.general.fns.link.LogLinkFunction;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

/**
 * Test linear reg.
 */
public class TestMultipleLinearRegression {

    @Test
    public void testBasic() {
        final MultipleLinearRegression reg = new MultipleLinearRegression(1);

        assertEquals(2, reg.getLongTermMemory().length);

        final LogLinkFunction lnk = new LogLinkFunction();
        reg.setLinkFunction(lnk);
        assertTrue(reg.getLinkFunction() == lnk);

        reg.getLongTermMemory()[0] = 1;
        reg.getLongTermMemory()[1] = 2;

        final double[] input = {1.0};
        final double[] output = reg.computeRegression(input);
        assertEquals(1, output.length);
        assertEquals(1.0986122886681098, output[0], AIFH.DEFAULT_PRECISION);
    }
}
