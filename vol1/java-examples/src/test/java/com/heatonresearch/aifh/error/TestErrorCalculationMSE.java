package com.heatonresearch.aifh.error;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * Test MSE error calc.
 */
public class TestErrorCalculationMSE {

    @Test
    public void testErrorCalc() {
        ErrorCalculation calc = new ErrorCalculationMSE();
        double result = ErrorTestingUtil.calculateError(
                calc,
                ErrorTestingUtil.ACTUAL,
                ErrorTestingUtil.IDEAL);
        assertEquals(151.6205, result, 0.001);
    }
}
