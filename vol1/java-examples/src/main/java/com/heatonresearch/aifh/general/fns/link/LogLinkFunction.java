package com.heatonresearch.aifh.general.fns.link;

import com.heatonresearch.aifh.AIFHError;
import com.heatonresearch.aifh.general.fns.Fn;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 8/26/13
 * Time: 5:46 PM
 * To change this template use File | Settings | File Templates.
 */
public class LogLinkFunction implements Fn {
    @Override
    public double evaluate(final double[] x) {
        if (x.length > 1) {
            throw new AIFHError("The logistic link function can only accept one parameter.");
        }
        return Math.log(x[0]);
    }
}
