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
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

/**
 * Test anneal learning.
 */
public class TestLearnAnneal {
    @Test
    public void testBasic() {
        final TrainAnneal anneal = new TrainAnneal(new TrialAlgo(), new TrialScore());
        assertEquals(400, anneal.coolingSchedule(), AIFH.DEFAULT_PRECISION);
    }

    @Test
    public void testGetStatus() {
        final TrainAnneal anneal = new TrainAnneal(new TrialAlgo(), new TrialScore());
        assertEquals("k=0,kMax=1000,t=0.0,prob=0.0", anneal.getStatus());
    }

    @Test
    public void testRandomize() {
        final TrialAlgo algo = new TrialAlgo();
        final TrainAnneal anneal = new TrainAnneal(algo, new TrialScore());
        anneal.performRandomize(algo.getLongTermMemory());
        anneal.finishTraining();
        assertEquals(0, algo.getLongTermMemory()[0], AIFH.DEFAULT_PRECISION);
    }

    @Test
    public void testIterations() {
        final TrainAnneal anneal = new TrainAnneal(new TrialAlgo(), new TrialScore(), 10, 400, 0.0001);
        anneal.setCycles(10);
        assertEquals(400, anneal.coolingSchedule(), AIFH.DEFAULT_PRECISION);


        assertEquals(400, anneal.getStartingTemperature(), AIFH.DEFAULT_PRECISION);
        assertEquals(0.0001, anneal.getEndingTemperature(), AIFH.DEFAULT_PRECISION);
        assertEquals(10, anneal.getCycles());

        assertEquals(0, anneal.getCurrentTemperature(), AIFH.DEFAULT_PRECISION);
        assertEquals(0, anneal.getK());
        assertEquals(false, anneal.done());
        assertEquals(true, Double.isInfinite(anneal.getLastError()));
        assertEquals(0, anneal.getLastProbability(), AIFH.DEFAULT_PRECISION);
        anneal.iteration();

        assertTrue(anneal.getLastError() > 0);

        assertEquals(87.46896591546223, anneal.getCurrentTemperature(), AIFH.DEFAULT_PRECISION);
        assertEquals(1, anneal.getK());
        assertEquals(false, anneal.done());

        for (int i = 0; i < 9; i++) {
            anneal.iteration();
        }

        assertEquals(true, anneal.done());
        assertEquals(9.999999999999E-5, anneal.getCurrentTemperature(), AIFH.DEFAULT_PRECISION);
        assertEquals(10, anneal.getK());

    }
}
