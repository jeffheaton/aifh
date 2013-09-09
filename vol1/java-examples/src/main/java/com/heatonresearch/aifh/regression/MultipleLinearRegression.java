/*
 * Artificial Intelligence for Humans
 * Volume 1: Fundamental Algorithms
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh

 * Copyright 2013 by Jeff Heaton
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * For more information on Heaton Research copyrights, licenses
 * and trademarks visit:
 * http://www.heatonresearch.com/copyright
 */

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

    public MultipleLinearRegression(final int theInputCount) {
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

        final double[] result = new double[1];
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
