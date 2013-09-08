package com.heatonresearch.aifh.error;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * Test ESS errors.
 */
public class TestErrorCalculationESS {

    @Test
    public void testErrorCalc() {
        ErrorCalculation calc = new ErrorCalculationESS();
        double result = ErrorTestingUtil.calculateError(
                calc,
                ErrorTestingUtil.ACTUAL,
                ErrorTestingUtil.IDEAL);
        assertEquals(1516.205, result, 0.001);
    }
}
