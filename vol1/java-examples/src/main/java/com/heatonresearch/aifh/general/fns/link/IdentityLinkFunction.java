package com.heatonresearch.aifh.general.fns.link;

import com.heatonresearch.aifh.AIFHError;
import com.heatonresearch.aifh.general.fns.Fn;

/**
 * The identity link function simply returns the first element of the vector it is passed.
 * Link functions can only accept vectors of size 1.
 */
public class IdentityLinkFunction implements Fn {
    /**
     * {@inheritDoc}
     */
    @Override
    public double evaluate(final double[] x) {
        if (x.length > 1) {
            throw new AIFHError("The linear link function can only accept one parameter.");
        }

        return x[0];
    }
}
