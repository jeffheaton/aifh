package com.heatonresearch.aifh.general.fns.link;

import com.heatonresearch.aifh.AIFH;
import com.heatonresearch.aifh.AIFHError;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * Test the Logit link function.
 */
public class TestLogitLinkFunction {
    @Test
    public void testEvaluate() {
        LogitLinkFunction fn = new LogitLinkFunction();
        double[] x = {2};
        double y = fn.evaluate(x);
        assertEquals(0.8807970779778823, y, AIFH.DEFAULT_PRECISION);
    }

    @Test(expected = AIFHError.class)
    public void testException() {
        LogitLinkFunction fn = new LogitLinkFunction();
        double[] x = {1, 2};
        fn.evaluate(x);
    }
}
