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
        DiscreteAnnealSubclass anneal = new DiscreteAnnealSubclass(1000, 4000, 1);
        assertEquals("k=0,kMax=1000,t=0.0,prob=0.0", anneal.getStatus());
    }

    @Test
    public void testGeneral() {
        DiscreteAnnealSubclass anneal = new DiscreteAnnealSubclass(1000, 4000, 1);
        anneal.setCycles(100);
        assertEquals(100, anneal.getCycles());
        assertEquals(0, anneal.getK());
        assertEquals(false, anneal.done());
    }

    @Test
    public void testCoolingSchedule() {
        DiscreteAnnealSubclass anneal = new DiscreteAnnealSubclass(1000, 400, 1);
        assertEquals(400, anneal.coolingSchedule(), AIFH.DEFAULT_PRECISION);
        anneal.iteration();
        assertEquals(397.61057939346017, anneal.coolingSchedule(), AIFH.DEFAULT_PRECISION);
    }

    @Test
    public void testProbability() {
        DiscreteAnnealSubclass anneal = new DiscreteAnnealSubclass(1000, 400, 1);
        assertEquals(0.9753099120283326, anneal.calcProbability(10, 20, anneal.coolingSchedule()), AIFH.DEFAULT_PRECISION);
        anneal.iteration();
        assertEquals(0.9751633961486054, anneal.calcProbability(10, 20, anneal.coolingSchedule()), AIFH.DEFAULT_PRECISION);
    }

    @Test
    public void testRun() {
        DiscreteAnnealSubclass anneal = new DiscreteAnnealSubclass(1000, 400, 1);
        while (!anneal.done()) {
            anneal.iteration();
        }

        CalculateDistance dist = new EuclideanDistance();

        assertEquals(1000, anneal.getK());
        assertEquals(0, dist.calculate(anneal.getBest(), DiscreteAnnealSubclass.IDEAL), AIFH.DEFAULT_PRECISION);
        assertEquals(0, anneal.getBestScore(), AIFH.DEFAULT_PRECISION);
    }


}
