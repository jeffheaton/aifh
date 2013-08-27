package com.heatonresearch.aifh.general.fns.link;

import com.heatonresearch.aifh.AIFHError;
import com.heatonresearch.aifh.general.fns.Fn;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 8/19/13
 * Time: 9:12 AM
 * To change this template use File | Settings | File Templates.
 */
public class LogitLinkFunction implements Fn {

    @Override
    public double evaluate(final double[] x) {
        if (x.length > 1) {
            throw new AIFHError("The logistic link function can only accept one parameter.");
        }
        return 1.0 / (1.0 + Math.exp(-x[0]));
    }
}
