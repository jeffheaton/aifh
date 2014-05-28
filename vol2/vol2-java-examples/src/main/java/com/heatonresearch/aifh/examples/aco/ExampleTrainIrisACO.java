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
package com.heatonresearch.aifh.examples.aco;

import com.heatonresearch.aifh.aco.ContinuousACO;
import com.heatonresearch.aifh.examples.util.SimpleLearn;
import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.learning.RBFNetwork;
import com.heatonresearch.aifh.learning.score.ScoreFunction;
import com.heatonresearch.aifh.learning.score.ScoreRegressionData;
import com.heatonresearch.aifh.normalize.DataSet;

import java.io.InputStream;
import java.util.List;
import java.util.Map;

/**
 * This example uses continuous ACO to fit an RBF network to the Iris data set.
 */
public class ExampleTrainIrisACO extends SimpleLearn {

    /**
     * Main entry point.
     *
     * @param args Not used.
     */
    public static void main(final String[] args) {
        final ExampleTrainIrisACO prg = new ExampleTrainIrisACO();
        prg.process();
    }

    /**
     * Run the example.
     */
    public void process() {
        try {
            final InputStream istream = this.getClass().getResourceAsStream("/iris.csv");
            if (istream == null) {
                System.out.println("Cannot access data set, make sure the resources are available.");
                System.exit(1);
            }

            final DataSet ds = DataSet.load(istream);
            // The following ranges are setup for the Iris data set.  If you wish to normalize other files you will
            // need to modify the below function calls other files.
            ds.normalizeRange(0, -1, 1);
            ds.normalizeRange(1, -1, 1);
            ds.normalizeRange(2, -1, 1);
            ds.normalizeRange(3, -1, 1);
            final Map<String, Integer> species = ds.encodeOneOfN(4);
            istream.close();

            RBFNetwork network = new RBFNetwork(4, 4, 3);

            final List<BasicData> trainingData = ds.extractSupervised(0, 4, 4, 3);

            ScoreFunction score = new ScoreRegressionData(trainingData);

            ContinuousACO train = new ContinuousACO(network, score, 30);

            performIterations(train, 100000, 0.05, true);

            train.finishTraining();

            queryOneOfN(network, trainingData, species);


        } catch (Throwable t) {
            t.printStackTrace();
        }
    }
}
