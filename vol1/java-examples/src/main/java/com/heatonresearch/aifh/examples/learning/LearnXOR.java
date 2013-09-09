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
import com.heatonresearch.aifh.learning.RBFNetwork;
import com.heatonresearch.aifh.learning.TrainGreedyRandom;
import com.heatonresearch.aifh.learning.score.ScoreFunction;
import com.heatonresearch.aifh.learning.score.ScoreRegressionData;

import java.util.List;

/**
 * Learn the XOR function with a RBF Network trained by Greedy Random.
 */
public class LearnXOR extends SimpleLearn {

    /**
     * The input necessary for XOR.
     */
    public static final double[][] XOR_INPUT = {{0.0, 0.0}, {1.0, 0.0},
            {0.0, 1.0}, {1.0, 1.0}};

    /**
     * The ideal data necessary for XOR.
     */
    public static final double[][] XOR_IDEAL = {{0.0}, {1.0}, {1.0}, {0.0}};

    /**
     * Perform the example.
     */
    public void process() {
        final List<BasicData> trainingData = BasicData.convertArrays(XOR_INPUT, XOR_IDEAL);
        final RBFNetwork network = new RBFNetwork(2, 5, 1);
        final ScoreFunction score = new ScoreRegressionData(trainingData);
        final TrainGreedyRandom train = new TrainGreedyRandom(true, network, score);
        performIterations(train, 1000000, 0.01, true);
        query(network, trainingData);
    }

    /**
     * The main method.
     *
     * @param args Not used.
     */
    public static void main(final String[] args) {
        final LearnXOR prg = new LearnXOR();
        prg.process();
    }
}
