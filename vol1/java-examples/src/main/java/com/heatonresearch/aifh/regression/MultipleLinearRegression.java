package com.heatonresearch.aifh.regression;

import com.heatonresearch.aifh.general.fns.Fn;
import com.heatonresearch.aifh.general.fns.link.IdentityLinkFunction;
import com.heatonresearch.aifh.learning.RegressionAlgorithm;


/**
 * Implements a multi-input linear regression function, with an optional link function.  By default the link function
 * is the identity function, which implements regular linear regression.  Setting the link function to other function
 * types allows you to create other Generalized Linear Models(GLMs).
 * <p/>
 * The long term memory is always of one greater length than the number of inputs.  The first memory element is the
 * intercept, and the others are coefficients to the inputs.
 * <p/>
 * For simple Linear Regression you should train with TrainLeastSquares.  If you are using a GLM, then you must
 * train with Reweight Least Squares.
 * <p/>
 * http://en.wikipedia.org/wiki/Linear_regression
 * http://en.wikipedia.org/wiki/Generalized_linear_model
 */
public class MultipleLinearRegression implements RegressionAlgorithm {

    /**
     * The long term memory, in this case coefficients to the linear regression.
     */
    private final double[] longTermMemory;

    /**
     * The link function to use.
     */
    private Fn linkFunction = new IdentityLinkFunction();

    public MultipleLinearRegression(int theInputCount) {
        this.longTermMemory = new double[theInputCount + 1];
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double[] computeRegression(final double[] input) {
        double sum = 0;

        for (int i = 1; i < this.longTermMemory.length; i++) {
            sum += input[i - 1] * this.longTermMemory[i];
        }
        sum += this.longTermMemory[0];

        double[] result = new double[1];
        result[0] = sum;
        result[0] = this.linkFunction.evaluate(result);
        return result;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double[] getLongTermMemory() {
        return this.longTermMemory;
    }

    /**
     * @return The link function.
     */
    public Fn getLinkFunction() {
        return linkFunction;
    }

    /**
     * Set the link function.
     *
     * @param linkFunction The link function.
     */
    public void setLinkFunction(final Fn linkFunction) {
        this.linkFunction = linkFunction;
    }
}