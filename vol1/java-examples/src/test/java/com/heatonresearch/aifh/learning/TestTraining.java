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

import static org.junit.Assert.*;

/**
 * Test training.
 */
public class TestTraining {

    private void performTest(final LearningMethod train) {

        assertFalse(train.done());

        train.getStatus();
        train.iteration();
        final double startError = train.getLastError();

        for (int i = 0; i < 1000 && !train.done(); i++) {
            train.iteration();
        }

        // make sure one last iteration does not blow up(if done was true)
        train.iteration();

        train.finishTraining();
        assertTrue((train.getLastError() < startError) || Math.abs(train.getLastError()) < 1);
    }


    @Test
    public void testAnneal() {
        final TrainAnneal anneal = new TrainAnneal(new TrialAlgo(), new TrialScore());
        performTest(anneal);
    }

    @Test
    public void testGreedyRandom() {
        final TrainGreedyRandom train = new TrainGreedyRandom(true, new TrialAlgo(), new TrialScore());

        train.setLowRange(0);
        train.setHighRange(10);

        assertEquals(0, train.getLowRange(), AIFH.DEFAULT_PRECISION);
        assertEquals(10, train.getHighRange(), AIFH.DEFAULT_PRECISION);

        performTest(train);
    }

    @Test
    public void testHillClimbing() {
        final TrainHillClimb train = new TrainHillClimb(true, new TrialAlgo(), new TrialScore());
        performTest(train);
    }

    @Test
    public void testNelderMead() {
        final TrainNelderMead train = new TrainNelderMead(new TrialAlgo(), new TrialScore());
        performTest(train);
    }
}
