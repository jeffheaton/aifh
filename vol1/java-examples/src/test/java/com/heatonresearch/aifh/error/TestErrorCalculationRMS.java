package com.heatonresearch.aifh.error;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * Test RMS error calc.
 */
public class TestErrorCalculationRMS {


    @Test
    public void testErrorCalc() {
        ErrorCalculation calc = new ErrorCalculationRMS();
        double result = ErrorTestingUtil.calculateError(
                calc,
                ErrorTestingUtil.ACTUAL,
                ErrorTestingUtil.IDEAL);
        assertEquals(12.3134, result, 0.001);
    }
}
