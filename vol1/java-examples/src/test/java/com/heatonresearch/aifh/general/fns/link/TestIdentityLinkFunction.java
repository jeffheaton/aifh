package com.heatonresearch.aifh.general.fns.link;

import com.heatonresearch.aifh.AIFH;
import com.heatonresearch.aifh.AIFHError;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 8/27/13
 * Time: 4:24 AM
 * To change this template use File | Settings | File Templates.
 */
public class TestIdentityLinkFunction {

    @Test
    public void testEvaluate() {
        IdentityLinkFunction fn = new IdentityLinkFunction();
        double[] x = {2};
        double y = fn.evaluate(x);
        assertEquals(2, y, AIFH.DEFAULT_PRECISION);
    }

    @Test(expected = AIFHError.class)
    public void testException() {
        IdentityLinkFunction fn = new IdentityLinkFunction();
        double[] x = {1, 2};
        fn.evaluate(x);
    }
}
