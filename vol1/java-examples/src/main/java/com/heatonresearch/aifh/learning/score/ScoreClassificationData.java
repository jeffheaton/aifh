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

import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.learning.ClassificationAlgorithm;
import com.heatonresearch.aifh.learning.MachineLearningAlgorithm;

import java.util.List;

/**
 * Score classification data. The score is the percentage cases that are wrong.
 * There is no "partial credit" or closeness.  A case is either right or wrong.
 */
public class ScoreClassificationData implements ScoreFunction {
    /**
     * The training data.
     */
    private final List<BasicData> trainingData;

    /**
     * Construct the score function.
     *
     * @param theTrainingData The training data.
     */
    public ScoreClassificationData(final List<BasicData> theTrainingData) {
        this.trainingData = theTrainingData;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double calculateScore(final MachineLearningAlgorithm algo) {
        int incorrectCount = 0;
        int totalCount = 0;

        final ClassificationAlgorithm ralgo = (ClassificationAlgorithm) algo;

        for (final BasicData aTrainingData : this.trainingData) {
            totalCount++;
            final int output = ralgo.computeClassification(aTrainingData.getInput());

            if (output != (int) aTrainingData.getIdeal()[0]) {
                incorrectCount++;
            }
        }

        return (double) incorrectCount / (double) totalCount;
    }
}
