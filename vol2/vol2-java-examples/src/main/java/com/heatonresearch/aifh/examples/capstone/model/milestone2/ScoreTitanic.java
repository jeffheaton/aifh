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
package com.heatonresearch.aifh.examples.capstone.model.milestone2;

import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.learning.MLMethod;
import com.heatonresearch.aifh.learning.RegressionAlgorithm;
import com.heatonresearch.aifh.learning.score.ScoreFunction;

import java.util.List;

/**
 * Score the Titanic model. The score is percentage cases predicted correctly.
 */
public class ScoreTitanic implements ScoreFunction {

    /**
     * The training data.
     */
    private final List<BasicData> trainingData;

    /**
     * Construct the score function.
     *
     * @param theTrainingData The training data.
     */
    public ScoreTitanic(final List<BasicData> theTrainingData) {
        this.trainingData = theTrainingData;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double calculateScore(final MLMethod algo) {
        int incorrectCount = 0;
        int totalCount = 0;

        final RegressionAlgorithm alg = (RegressionAlgorithm) algo;

        for (final BasicData aTrainingData : this.trainingData) {
            totalCount++;
            boolean predictSurvive = alg.computeRegression(aTrainingData.getInput())[0] > 0.5;
            boolean idealSurvive = aTrainingData.getIdeal()[0] > 0.5;

            if (predictSurvive == idealSurvive) {
                incorrectCount++;
            }
        }

        return (double) incorrectCount / (double) totalCount;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean shouldMinimize() {
        return false;
    }
}
