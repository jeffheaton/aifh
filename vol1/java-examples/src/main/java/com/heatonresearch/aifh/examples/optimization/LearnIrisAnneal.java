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

package com.heatonresearch.aifh.examples.optimization;

import com.heatonresearch.aifh.examples.learning.SimpleLearn;
import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.learning.RBFNetwork;
import com.heatonresearch.aifh.learning.score.ScoreFunction;
import com.heatonresearch.aifh.learning.score.ScoreRegressionData;
import com.heatonresearch.aifh.normalize.DataSet;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

import java.io.InputStream;
import java.util.List;
import java.util.Map;

/**
 * Use a RBF network to learn the Iris data set, trained by hill climbing.
 */
public class LearnIrisAnneal extends SimpleLearn {
    /**
     * Run the example.
     */
    public void process() {
        try {
            final InputStream istream = this.getClass().getResourceAsStream("/iris.csv");

            final DataSet ds = DataSet.load(istream);
            // The following ranges are setup for the Iris data set.  If you wish to normalize other files you will
            // need to modify the below function calls other files.
            ds.normalizeRange(0, 0, 1);
            ds.normalizeRange(1, 0, 1);
            ds.normalizeRange(2, 0, 1);
            ds.normalizeRange(3, 0, 1);
            final Map<String, Integer> species = ds.encodeOneOfN(4);
            istream.close();

            final List<BasicData> trainingData = ds.extractSupervised(0, 4, 4, 3);

            final RBFNetwork network = new RBFNetwork(4, 4, 3);
            network.reset(new MersenneTwisterGenerateRandom());
            double[] wt = {-1.1292562663220111, -10.259748633327407, -8.498054186542086, 0.1459025339896104, 4.387940603766271, 0.9218114191463057, -9.162995017336574, 4.2010355827113965, 7.521548600863678, -2.557303701281194, -13.293073260128251, -5.537310822264542, -8.489791085114677, 1.0356537033076063, -4.978111843668845, 1.032262453276176, 0.5206980272087254, -7.2938235050702405, -2.423732253951989, -14.147415867016894, 8.378484803753938, -7.5837938090572115, -0.14348203769478707, -10.575543067411726, -10.560112296041243, 4.642362652296021, -10.578109121756368, -13.035444531010937, 7.547431474410416, 8.60746743204411, 23.719705429865265, -2.506332280360064, -6.564320025544821, 1.6662091815253912, -16.17914353520914, 0.5892505577958975, -7.439494685395096, 5.25599933060961, 2.570384852094083, -1.6991288239875981, -0.8480185879464069, -5.490825674643135, -6.205775554687417, -3.370820111415552, 6.65782726566459, 1.7415728189815887, -8.221204392843346, 2.8403925370500547, 2.7325289542485076, 1.4867230218228877, -0.3297240727328973};
            System.arraycopy(wt, 0, network.getLongTermMemory(), 0, wt.length);
            ScoreFunction score = new ScoreRegressionData(trainingData);
            System.out.println(score.calculateScore(network));



            /*final ScoreFunction score = new ScoreRegressionData(trainingData);
            final TrainAnneal train = new TrainAnneal(network, score);
            performIterations(train, 100000, 0.01, true);
            queryOneOfN(network, trainingData, species);
            System.out.println(Arrays.toString(network.getLongTermMemory()));  */


        } catch (Throwable t) {
            t.printStackTrace();
        }


    }

    /**
     * The main method.
     *
     * @param args Not used.
     */
    public static void main(final String[] args) {
        final LearnIrisAnneal prg = new LearnIrisAnneal();
        prg.process();
    }
}
