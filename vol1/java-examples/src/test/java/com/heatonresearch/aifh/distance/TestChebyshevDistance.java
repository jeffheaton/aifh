package com.heatonresearch.aifh.distance;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 7/13/13
 * Time: 6:13 PM
 * To change this template use File | Settings | File Templates.
 */
public class TestChebyshevDistance {
    @Test
    public void testDistanceCalc() {
        CalculateDistance calc = new ChebyshevDistance();
        double[] pos1 = {0.5, 1.0, 2.5,};
        double[] pos2 = {0.1, 2.0, -2.5,};

        assertEquals(5.0, calc.calculate(pos1, pos2), 0.001);
    }
}
