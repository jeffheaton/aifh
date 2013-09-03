package com.heatonresearch.aifh.general.fns;

import com.heatonresearch.aifh.AIFH;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 8/27/13
 * Time: 4:14 AM
 * To change this template use File | Settings | File Templates.
 */
public class TestInverseMultiquadricFunction {
    @Test
    public void testEvaluate() {
        double[] params = {5, 0, 0, 0};
        InverseMultiquadricFunction funct = new InverseMultiquadricFunction(3, params, 0);
        double[] x = {-1, 0, 1};
        double y = funct.evaluate(x);
        assertEquals(0.11396057645963795, y, AIFH.DEFAULT_PRECISION);

    }

    @Test
    public void testToString() {
        double[] params = {5, 0, 0, 0};
        InverseMultiquadricFunction funct = new InverseMultiquadricFunction(3, params, 0);
        double[] x = {-1, 0, 1};
        funct.evaluate(x);
        assertEquals("[InverseMultiquadricFunction:width=5.00,center=,0.00,0.00]", funct.toString());
    }

    @Test
    public void testOther() {
        double[] params = {5, 0, 0, 0};
        InverseMultiquadricFunction funct = new InverseMultiquadricFunction(3, params, 0);
        assertEquals(3, funct.getDimensions());
        funct.setCenter(0, 100);
        assertEquals(100, funct.getCenter(0), AIFH.DEFAULT_PRECISION);
        funct.setWidth(5);
        assertEquals(5, funct.getWidth(), AIFH.DEFAULT_PRECISION);
    }
}
