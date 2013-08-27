package com.heatonresearch.aifh.general.fns.link;

import com.heatonresearch.aifh.AIFH;
import com.heatonresearch.aifh.AIFHError;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 8/27/13
 * Time: 4:34 AM
 * To change this template use File | Settings | File Templates.
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
        double y = fn.evaluate(x);
    }
}
