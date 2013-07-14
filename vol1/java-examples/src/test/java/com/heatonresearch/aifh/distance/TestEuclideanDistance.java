package com.heatonresearch.aifh.distance;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class TestEuclideanDistance {

    @Test
    public void testDistanceCalc() {
        CalculateDistance calc = new EuclideanDistance();
        double[] pos1 = {0.5, 1.0, 2.5,};
        double[] pos2 = {0.1, 2.0, -2.5,};

        assertEquals(5.1146, calc.calculate(pos1, pos2), 0.001);
    }
}
