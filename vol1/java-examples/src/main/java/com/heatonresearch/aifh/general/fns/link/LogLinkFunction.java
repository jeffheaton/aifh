package com.heatonresearch.aifh.general.fns.link;

import com.heatonresearch.aifh.AIFHError;
import com.heatonresearch.aifh.general.fns.Fn;

/**
 * The log link function for a GLM.
 * <p/>
 * http://en.wikipedia.org/wiki/Generalized_linear_model
 */
public class LogLinkFunction implements Fn {

    /**
     * {@inheritDoc}
     */
    @Override
    public double evaluate(final double[] x) {
        if (x.length > 1) {
            throw new AIFHError("The logistic link function can only accept one parameter.");
        }
        return Math.log(x[0]);
    }
}
