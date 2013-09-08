package com.heatonresearch.aifh.general.fns.link;

import com.heatonresearch.aifh.AIFH;
import com.heatonresearch.aifh.AIFHError;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * Test the log link function.
 */
public class TestLogLinkFunction {
    @Test
    public void testEvaluate() {
        LogLinkFunction fn = new LogLinkFunction();
        double[] x = {2};
        double y = fn.evaluate(x);
        assertEquals(0.6931471805599453, y, AIFH.DEFAULT_PRECISION);
    }

    @Test(expected = AIFHError.class)
    public void testException() {
        LogLinkFunction fn = new LogLinkFunction();
        double[] x = {1, 2};
        fn.evaluate(x);
    }
}
