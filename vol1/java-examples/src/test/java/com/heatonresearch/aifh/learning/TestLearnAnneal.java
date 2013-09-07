package com.heatonresearch.aifh.learning;

import com.heatonresearch.aifh.AIFH;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 8/31/13
 * Time: 11:50 AM
 * To change this template use File | Settings | File Templates.
 */
public class TestLearnAnneal {
    @Test
    public void testBasic() {
        TrainAnneal anneal = new TrainAnneal(new TrialAlgo(), new TrialScore());
        assertEquals(400, anneal.coolingSchedule(), AIFH.DEFAULT_PRECISION);
    }

    @Test
    public void testGetStatus() {
        TrainAnneal anneal = new TrainAnneal(new TrialAlgo(), new TrialScore());
        assertEquals("k=0,kMax=1000,t=0.0,prob=0.0", anneal.getStatus());
    }

    @Test
    public void testRandomize() {
        TrialAlgo algo = new TrialAlgo();
        TrainAnneal anneal = new TrainAnneal(algo, new TrialScore());
        anneal.performRandomize(algo.getLongTermMemory());
        anneal.finishTraining();
        assertEquals(0, algo.getLongTermMemory()[0], AIFH.DEFAULT_PRECISION);
    }

    @Test
    public void testIterations() {
        TrainAnneal anneal = new TrainAnneal(new TrialAlgo(), new TrialScore(), 10, 400, 0.0001);
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
