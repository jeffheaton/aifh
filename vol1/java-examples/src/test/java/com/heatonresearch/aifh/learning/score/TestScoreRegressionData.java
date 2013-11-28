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

import com.heatonresearch.aifh.AIFH;
import com.heatonresearch.aifh.error.ErrorCalculation;
import com.heatonresearch.aifh.error.ErrorCalculationSSE;
import com.heatonresearch.aifh.general.data.BasicData;
import org.junit.Test;

import java.util.List;

import static org.junit.Assert.assertEquals;

/**
 * Test regression data.
 */
public class TestScoreRegressionData {

    public static final double[][] TEST_INPUT = {
            {0.0, 0.0},
            {1.0, 0.0},
            {0.0, 1.0},
            {1.0, 1.0}
    };

    public static final double[][] TEST_IDEAL = {
            {0.0},
            {1.0},
            {1.0},
            {0.0}
    };

    @Test
    public void testGeneral() {
        final List<BasicData> training = BasicData.convertArrays(TEST_INPUT, TEST_IDEAL);
        final ScoreRegressionData score = new ScoreRegressionData(training);
        final ErrorCalculation ec = new ErrorCalculationSSE();
        score.setErrorCalc(ec);
        assertEquals(ec, score.getErrorCalc());
    }

    @Test
    public void testRegression() {
        final double[] ACTUAL = {0.0, 1.0, 0.0, 0.0};
        final List<BasicData> training = BasicData.convertArrays(TEST_INPUT, TEST_IDEAL);
        final ScoreRegressionData score = new ScoreRegressionData(training);
        final SimpleAlgo simple = new SimpleAlgo(ACTUAL);
        final double s = score.calculateScore(simple);
        assertEquals(training, score.getTrainingData());
        assertEquals(1.0, s, AIFH.DEFAULT_PRECISION);
    }
}
