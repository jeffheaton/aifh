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

package com.heatonresearch.aifh.examples.learning;

import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.learning.TrainGreedyRandom;
import com.heatonresearch.aifh.learning.score.ScoreFunction;
import com.heatonresearch.aifh.learning.score.ScoreRegressionData;

import java.util.ArrayList;
import java.util.List;

/**
 * Learn a simple polynomial with the Greedy Random algorithm.
 */
public class LearnPolynomial extends SimpleLearn {

    public List<BasicData> generateTrainingData() {
        final List<BasicData> result = new ArrayList<BasicData>();

        for (double x = -50; x < 50; x++) {
            final double y = (2 * Math.pow(x, 2)) + (4 * x) + 6;
            final BasicData pair = new BasicData(1, 1);
            pair.getInput()[0] = x;
            pair.getIdeal()[0] = y;
            result.add(pair);
        }

        return result;
    }


    /**
     * Run the example.
     */
    public void process() {
        final List<BasicData> trainingData = generateTrainingData();
        final PolynomialFn poly = new PolynomialFn(3);
        final ScoreFunction score = new ScoreRegressionData(trainingData);
        final TrainGreedyRandom train = new TrainGreedyRandom(true, poly, score);
        performIterations(train, 1000000, 0.01, true);
        System.out.println(poly.toString());
    }

    /**
     * The main method.
     *
     * @param args Not used.
     */
    public static void main(final String[] args) {
        final LearnPolynomial prg = new LearnPolynomial();
        prg.process();
    }
}
