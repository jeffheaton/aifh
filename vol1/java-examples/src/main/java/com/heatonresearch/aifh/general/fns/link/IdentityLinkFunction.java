package com.heatonresearch.aifh.general.fns.link;

import com.heatonresearch.aifh.AIFHError;
import com.heatonresearch.aifh.general.fns.Fn;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 8/19/13
 * Time: 9:32 AM
 * To change this template use File | Settings | File Templates.
 */
public class IdentityLinkFunction implements Fn {
    @Override
    public double evaluate(final double[] x) {
        if (x.length > 1) {
            throw new AIFHError("The linear link function can only accept one parameter.");
        }

        return x[0];
    }
}
