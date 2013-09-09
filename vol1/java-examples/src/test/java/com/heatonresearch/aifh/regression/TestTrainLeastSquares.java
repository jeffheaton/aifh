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

import com.heatonresearch.aifh.AIFH;
import com.heatonresearch.aifh.general.data.BasicData;
import org.junit.Test;

import java.util.List;

import static org.junit.Assert.assertEquals;

/**
 * Test Least squares.
 */
public class TestTrainLeastSquares {
    @Test
    public void testTrain() {

        final double[][] x = {
                {5, 10, 2},
                {10, 20, 4},
                {15, 30, 6},
                {20, 40, 8},
                {25, 50, 10}};

        final double[][] y = {
                {70},
                {132},
                {194},
                {256},
                {318}
        };


        final List<BasicData> trainingData = BasicData.convertArrays(x, y);
        final MultipleLinearRegression regression = new MultipleLinearRegression(3);
        final TrainLeastSquares train = new TrainLeastSquares(regression, trainingData);
        train.iteration();

        assertEquals(8, regression.getLongTermMemory()[0], 0.0001);
        assertEquals(10.514285, regression.getLongTermMemory()[1], 0.0001);
        assertEquals(0.14285, regression.getLongTermMemory()[2], 0.0001);
        assertEquals(1.0, train.getR2(), 0.0001);
        assertEquals(0, train.getError(), AIFH.DEFAULT_PRECISION);

        for (int i = 0; i < x.length; i++) {
            final double[] output = regression.computeRegression(x[i]);
            assertEquals(y[i][0], output[0], 0.0001);
        }
    }

}

