/*
 * Artificial Intelligence for Humans
 * Volume 2: Nature Inspired Algorithms
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh
 *
 * Copyright 2014 by Jeff Heaton
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
package com.heatonresearch.aifh.examples.gp;

import com.heatonresearch.aifh.error.ErrorCalculation;
import com.heatonresearch.aifh.error.ErrorCalculationMSE;
import com.heatonresearch.aifh.evolutionary.genome.Genome;
import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.learning.MLMethod;
import com.heatonresearch.aifh.learning.RegressionAlgorithm;
import com.heatonresearch.aifh.learning.score.ScoreFunction;

import java.util.List;

/**
 * Score regression data.  The score is done using an error calculation method.
 */
public class ScoreSmallExpression implements ScoreFunction {

    /**
     * The error calculator.
     */
    private ErrorCalculation errorCalc = new ErrorCalculationMSE();

    /**
     * The training data.
     */
    private final List<BasicData> trainingData;

    /**
     * The maximum tree size.
     */
    private final int maxLength;

    /**
     * Construct the function.
     *
     * @param theTrainingData The training data.
     * @param theMaxLength The maximum tree size.
     */
    public ScoreSmallExpression(final List<BasicData> theTrainingData, int theMaxLength) {
        this.trainingData = theTrainingData;
        this.maxLength = theMaxLength;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double calculateScore(final MLMethod algo) {
        ErrorCalculation ec = this.errorCalc.create();

        final RegressionAlgorithm ralgo = (RegressionAlgorithm) algo;
        final Genome genome = (Genome)ralgo;

        if( genome.size()>this.maxLength ) {
            return Double.POSITIVE_INFINITY;
        }

        // evaulate
        ec.clear();
        for (final BasicData pair : this.trainingData) {
            final double[] output = ralgo.computeRegression(pair.getInput());
            ec.updateError(output, pair.getIdeal(), 1.0);
        }

        return ec.calculate();
    }

    /**
     * @return The error calculation method.
     */
    public ErrorCalculation getErrorCalc() {
        return errorCalc;
    }

    /**
     * Set the error calculation method.
     *
     * @param errorCalc The error calculation method.
     */
    public void setErrorCalc(final ErrorCalculation errorCalc) {
        this.errorCalc = errorCalc;
    }

    /**
     * @return The training data.
     */
    public List<BasicData> getTrainingData() {
        return trainingData;
    }

    /**
     * @return True, this scoring method seeks to minimize.
     */
    @Override
    public boolean shouldMinimize() {
        return true;
    }
}
