package com.heatonresearch.aifh.general.fns.link;

import com.heatonresearch.aifh.AIFH;
import com.heatonresearch.aifh.AIFHError;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * Test the inverse link function.
 */
public class TestInverseLinkFunction {
    @Test
    public void testEvaluate() {
        InverseLinkFunction fn = new InverseLinkFunction();
        double[] x = {2};
        double y = fn.evaluate(x);
        assertEquals(-0.5, y, AIFH.DEFAULT_PRECISION);
    }

    @Test(expected = AIFHError.class)
    public void testException() {
        InverseLinkFunction fn = new InverseLinkFunction();
        double[] x = {1, 2};
        fn.evaluate(x);
    }
}
