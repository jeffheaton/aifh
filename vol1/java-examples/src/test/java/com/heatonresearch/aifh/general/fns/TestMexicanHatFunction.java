package com.heatonresearch.aifh.general.fns;

import com.heatonresearch.aifh.AIFH;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * Test the Mexican hat function.
 */
public class TestMexicanHatFunction {
    @Test
    public void testEvaluate() {
        double[] params = {5, 0, 0, 0};
        MexicanHatFunction funct = new MexicanHatFunction(3, params, 0);
        double[] x = {-1, 0, 1};
        double y = funct.evaluate(x);
        assertEquals(-0.36787944117144233, y, AIFH.DEFAULT_PRECISION);

    }

    @Test
    public void testToString() {
        double[] params = {5, 0, 0, 0};
        MexicanHatFunction funct = new MexicanHatFunction(3, params, 0);
        double[] x = {-1, 0, 1};
        funct.evaluate(x);
        assertEquals("[MexicanHatFunction:width=5.00,center=,0.00,0.00]", funct.toString());
    }

    @Test
    public void testOther() {
        double[] params = {5, 0, 0, 0};
        MexicanHatFunction funct = new MexicanHatFunction(3, params, 0);
        assertEquals(3, funct.getDimensions());
        funct.setCenter(0, 100);
        assertEquals(100, funct.getCenter(0), AIFH.DEFAULT_PRECISION);
        funct.setWidth(5);
        assertEquals(5, funct.getWidth(), AIFH.DEFAULT_PRECISION);
    }
}
