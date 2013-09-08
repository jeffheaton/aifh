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
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 7/30/13
 * Time: 2:13 PM
 * To change this template use File | Settings | File Templates.
 */
public class ScoreRegressionData implements ScoreFunction {

    private ErrorCalculation errorCalc = new ErrorCalculationMSE();
    private final List<BasicData> trainingData;

    public ScoreRegressionData(List<BasicData> theTrainingData) {
        this.trainingData = theTrainingData;
    }

    @Override
    public double calculateScore(MachineLearningAlgorithm algo) {
        RegressionAlgorithm ralgo = (RegressionAlgorithm) algo;
        // evaulate
        errorCalc.clear();
        for (BasicData pair : this.trainingData) {
            double[] output = ralgo.computeRegression(pair.getInput());
            errorCalc.updateError(output, pair.getIdeal(), 1.0);
        }

        return errorCalc.calculate();
    }

    public ErrorCalculation getErrorCalc() {
        return errorCalc;
    }

    public void setErrorCalc(final ErrorCalculation errorCalc) {
        this.errorCalc = errorCalc;
    }

    public List<BasicData> getTrainingData() {
        return trainingData;
    }
}
