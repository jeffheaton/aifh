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

package com.heatonresearch.aifh.learning;

import com.heatonresearch.aifh.AIFH;
import com.heatonresearch.aifh.randomize.BasicGenerateRandom;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

/**
 * Test the RBF network.
 */
public class TestRBFNetwork {

    @Test
    public void testBasics() {
        final RBFNetwork network = new RBFNetwork(2, 1, 1);

        // should be 7, (2*1) + (1+(1 bias))*1 + 3 RBF params
        // 2 + 2 + 3 = 7
        assertEquals(7, network.getLongTermMemory().length);

        assertEquals("[RBFNetwork:inputCount=2,outputCount=1,RBFs=[[GaussianFunction:width=0.00,center=0.00,0.00]]]", network.toString());

    }

    @Test
    public void testResetCompute() {
        final RBFNetwork network = new RBFNetwork(2, 1, 1);
        double total = 0;
        for (int i = 0; i < network.getLongTermMemory().length; i++) {
            total += network.getLongTermMemory()[i];
        }
        assertEquals(0, total, AIFH.DEFAULT_PRECISION);

        network.reset(new BasicGenerateRandom());

        for (int i = 0; i < network.getLongTermMemory().length; i++) {
            total += network.getLongTermMemory()[i];
        }

        assertTrue(Math.abs(total) > AIFH.DEFAULT_PRECISION);

    }

    @Test
    public void testComputeRegression() {
        final RBFNetwork network = new RBFNetwork(2, 1, 1);

        final double[] ltm = {
                2.0,  // input 1 to RBF 1
                2.0,  // input 2 to RBF 1
                5.0,  // RBF width
                2.0,  // RBF, center-0
                4.0,  // RBF, center-1
                3.0,  // RBF1 to Output 1
                4.0};  // Bias to Output 1


        System.arraycopy(ltm, 0, network.getLongTermMemory(), 0, ltm.length);

        final double[] x = {1, 2};

        final double y = network.computeRegression(x)[0];

        // Inputs: (2*1) + (2*2) = 6
        // RBF: Gaussian(6) = 1
        // Outputs: (1*3) + (1*4) = 7
        assertEquals(7, y, AIFH.DEFAULT_PRECISION);
    }

    @Test
    public void testComputeClassification() {
        final RBFNetwork network = new RBFNetwork(2, 1, 2);

        final double[] ltm = {
                2.0,  // input 1 to RBF 1
                2.0,  // input 2 to RBF 1
                5.0,  // RBF width
                2.0,  // RBF, center-0
                4.0,  // RBF, center-1
                3.0,  // RBF1 to Output 1
                4.0,  // Bias to Output 1
                5.0,  // RBF1 to Output 2
                6.0}; // Bias to Output 2


        System.arraycopy(ltm, 0, network.getLongTermMemory(), 0, ltm.length);

        final double[] x = {1, 2};

        final double[] y = network.computeRegression(x);

        // Inputs: (2*1) + (2*2) = 6
        // RBF: Gaussian(6) = 1
        // Outputs: (1*3) + (1*4) = 7
        assertEquals(7, y[0], AIFH.DEFAULT_PRECISION);

        // Inputs: (2*1) + (2*2) = 6
        // RBF: Gaussian(6) = 1
        // Outputs: (1*5) + (1*6) = 11
        assertEquals(11, y[1], AIFH.DEFAULT_PRECISION);

        final int cls = network.computeClassification(x);

        // class 1 is higher than class 0
        assertEquals(1, cls);
    }
}
