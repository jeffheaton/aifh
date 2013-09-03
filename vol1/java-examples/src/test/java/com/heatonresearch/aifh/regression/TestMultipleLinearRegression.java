package com.heatonresearch.aifh.regression;

import com.heatonresearch.aifh.AIFH;
import com.heatonresearch.aifh.general.fns.link.LogLinkFunction;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 7/31/13
 * Time: 9:26 PM
 * To change this template use File | Settings | File Templates.
 */
public class TestMultipleLinearRegression {

    @Test
    public void testBasic() {
        MultipleLinearRegression reg = new MultipleLinearRegression(1);

        assertEquals(2, reg.getLongTermMemory().length);

        LogLinkFunction lnk = new LogLinkFunction();
        reg.setLinkFunction(lnk);
        assertTrue(reg.getLinkFunction() == lnk);

        reg.getLongTermMemory()[0] = 1;
        reg.getLongTermMemory()[1] = 2;

        double[] input = {1.0};
        double[] output = reg.computeRegression(input);
        assertEquals(1, output.length);
        assertEquals(1.0986122886681098, output[0], AIFH.DEFAULT_PRECISION);
    }
}
