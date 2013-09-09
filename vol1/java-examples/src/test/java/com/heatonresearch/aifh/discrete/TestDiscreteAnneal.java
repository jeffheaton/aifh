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

package com.heatonresearch.aifh.discrete;

import com.heatonresearch.aifh.AIFH;
import com.heatonresearch.aifh.distance.CalculateDistance;
import com.heatonresearch.aifh.distance.EuclideanDistance;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * Test the discrete anneal subclass.
 */
public class TestDiscreteAnneal {
    @Test
    public void testStatus() {
        final DiscreteAnnealSubclass anneal = new DiscreteAnnealSubclass(1000, 4000, 1);
        assertEquals("k=0,kMax=1000,t=0.0,prob=0.0", anneal.getStatus());
    }

    @Test
    public void testGeneral() {
        final DiscreteAnnealSubclass anneal = new DiscreteAnnealSubclass(1000, 4000, 1);
        anneal.setCycles(100);
        assertEquals(100, anneal.getCycles());
        assertEquals(0, anneal.getK());
        assertEquals(false, anneal.done());
    }

    @Test
    public void testCoolingSchedule() {
        final DiscreteAnnealSubclass anneal = new DiscreteAnnealSubclass(1000, 400, 1);
        assertEquals(400, anneal.coolingSchedule(), AIFH.DEFAULT_PRECISION);
        anneal.iteration();
        assertEquals(397.61057939346017, anneal.coolingSchedule(), AIFH.DEFAULT_PRECISION);
    }

    @Test
    public void testProbability() {
        final DiscreteAnnealSubclass anneal = new DiscreteAnnealSubclass(1000, 400, 1);
        assertEquals(0.9753099120283326, anneal.calcProbability(10, 20, anneal.coolingSchedule()), AIFH.DEFAULT_PRECISION);
        anneal.iteration();
        assertEquals(0.9751633961486054, anneal.calcProbability(10, 20, anneal.coolingSchedule()), AIFH.DEFAULT_PRECISION);
    }

    @Test
    public void testRun() {
        final DiscreteAnnealSubclass anneal = new DiscreteAnnealSubclass(1000, 400, 1);
        while (!anneal.done()) {
            anneal.iteration();
        }

        final CalculateDistance dist = new EuclideanDistance();

        assertEquals(1000, anneal.getK());
        assertEquals(0, dist.calculate(anneal.getBest(), DiscreteAnnealSubclass.IDEAL), AIFH.DEFAULT_PRECISION);
        assertEquals(0, anneal.getBestScore(), AIFH.DEFAULT_PRECISION);
    }


}
