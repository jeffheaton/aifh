package com.heatonresearch.aifh.general.fns.link;

import com.heatonresearch.aifh.AIFHError;
import com.heatonresearch.aifh.general.fns.Fn;

/**
 * The Logistic (logit) link function for a GLM.
 * <p/>
 * http://en.wikipedia.org/wiki/Generalized_linear_model
 */
public class LogitLinkFunction implements Fn {

    /**
     * {@inheritDoc}
     */
    @Override
    public double evaluate(final double[] x) {
        if (x.length > 1) {
            throw new AIFHError("The logistic link function can only accept one parameter.");
        }
        return 1.0 / (1.0 + Math.exp(-x[0]));
    }
}
