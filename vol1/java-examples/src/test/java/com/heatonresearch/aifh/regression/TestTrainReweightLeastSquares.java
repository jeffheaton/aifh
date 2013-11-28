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
import com.heatonresearch.aifh.general.fns.link.LogitLinkFunction;
import org.junit.Test;

import java.util.List;

import static org.junit.Assert.assertEquals;

/**
 * Test reweight squares.
 */
public class TestTrainReweightLeastSquares {
    @Test
    public void testTrain() {

        final double[][] x = {
                {1},
                {3},
                {2},
                {200},
                {230}};

        final double[][] y = {
                {1.0},
                {1.0},
                {1.0},
                {0.0},
                {0.0}
        };


        final List<BasicData> trainingData = BasicData.convertArrays(x, y);
        final MultipleLinearRegression regression = new MultipleLinearRegression(1);
        regression.setLinkFunction(new LogitLinkFunction());
        final TrainReweightLeastSquares train = new TrainReweightLeastSquares(regression, trainingData);
        train.iteration();
        train.getError();

        final double[] input = {0};
        final double[] output = regression.computeRegression(input);
        assertEquals(0.8833017302699877, output[0], AIFH.DEFAULT_PRECISION);
    }
}
