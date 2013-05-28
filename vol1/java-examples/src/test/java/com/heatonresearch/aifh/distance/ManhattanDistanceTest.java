package com.heatonresearch.aifh.distance;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 5/27/13
 * Time: 6:36 AM
 * To change this template use File | Settings | File Templates.
 */
public class ManhattanDistanceTest {
    @Test
    public void testDistanceCalc() {
        CalculateDistance calc = new ManhattanDistance();
        double[] pos1 = {0.5, 1.0, 2.5,};
        double[] pos2 = {0.1, 2.0, -2.5,};

        assertEquals(6.4, calc.calculate(pos1, pos2), 0.001);
    }
}
