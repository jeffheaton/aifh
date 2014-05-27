package com.heatonresearch.aifh.examples.aco;

import com.heatonresearch.aifh.aco.ContinuousACO;
import com.heatonresearch.aifh.examples.util.SimpleLearn;
import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.learning.RBFNetwork;
import com.heatonresearch.aifh.learning.score.ScoreFunction;
import com.heatonresearch.aifh.learning.score.ScoreRegressionData;
import com.heatonresearch.aifh.normalize.DataSet;
import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

import java.io.InputStream;
import java.util.List;
import java.util.Map;

/**
 * This example uses continuous ACO to fit an RBF network to the Iris data set.
 */
public class ExampleTrainIrisACO extends SimpleLearn {

    /**
     * Main entry point.
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

            RBFNetwork network = new RBFNetwork(4,4,3);

            final List<BasicData> trainingData = ds.extractSupervised(0, 4, 4, 3);

            ScoreFunction score = new ScoreRegressionData(trainingData);

            ContinuousACO train = new ContinuousACO(network,score,30);

            performIterations(train, 100000, 0.05, true);

            train.finishTraining();

            queryOneOfN(network, trainingData, species);


        } catch (Throwable t) {
            t.printStackTrace();
        }
    }
}
