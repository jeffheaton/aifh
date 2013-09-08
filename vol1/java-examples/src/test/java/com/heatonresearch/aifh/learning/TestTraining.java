package com.heatonresearch.aifh.learning;

import com.heatonresearch.aifh.AIFH;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Test training.
 */
public class TestTraining {

    private void performTest(LearningAlgorithm train) {

        assertFalse(train.done());

        train.getStatus();
        train.iteration();
        double startError = train.getLastError();

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
        TrainAnneal anneal = new TrainAnneal(new TrialAlgo(), new TrialScore());
        performTest(anneal);
    }

    @Test
    public void testGreedyRandom() {
        TrainGreedyRandom train = new TrainGreedyRandom(true, new TrialAlgo(), new TrialScore());

        train.setLowRange(0);
        train.setHighRange(10);

        assertEquals(0, train.getLowRange(), AIFH.DEFAULT_PRECISION);
        assertEquals(10, train.getHighRange(), AIFH.DEFAULT_PRECISION);

        performTest(train);
    }

    @Test
    public void testHillClimbing() {
        TrainHillClimb train = new TrainHillClimb(true, new TrialAlgo(), new TrialScore());
        performTest(train);
    }

    @Test
    public void testNelderMead() {
        TrainNelderMead train = new TrainNelderMead(new TrialAlgo(), new TrialScore());
        performTest(train);
    }
}
