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

package com.heatonresearch.aifh.learning.score;

import com.heatonresearch.aifh.error.ErrorCalculation;
import com.heatonresearch.aifh.error.ErrorCalculationMSE;
import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.learning.MachineLearningAlgorithm;
import com.heatonresearch.aifh.learning.RegressionAlgorithm;

import java.util.List;

/**
 * Score regression data.  The score is done using an error calculation method.
 */
public class ScoreRegressionData implements ScoreFunction {

    /**
     * The error calculator.
     */
    private ErrorCalculation errorCalc = new ErrorCalculationMSE();

    /**
     * The training data.
     */
    private final List<BasicData> trainingData;

    /**
     * Construct the function.
     *
     * @param theTrainingData The training data.
     */
    public ScoreRegressionData(final List<BasicData> theTrainingData) {
        this.trainingData = theTrainingData;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double calculateScore(final MachineLearningAlgorithm algo) {
        final RegressionAlgorithm ralgo = (RegressionAlgorithm) algo;
        // evaulate
        errorCalc.clear();
        for (final BasicData pair : this.trainingData) {
            final double[] output = ralgo.computeRegression(pair.getInput());
            errorCalc.updateError(output, pair.getIdeal(), 1.0);
        }

        return errorCalc.calculate();
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
}
