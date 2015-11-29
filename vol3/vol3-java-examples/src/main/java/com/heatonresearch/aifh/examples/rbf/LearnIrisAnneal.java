package com.heatonresearch.aifh.examples.rbf;

import com.heatonresearch.aifh.examples.learning.SimpleLearn;
import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.learning.RBFNetwork;
import com.heatonresearch.aifh.learning.TrainAnneal;
import com.heatonresearch.aifh.learning.score.ScoreFunction;
import com.heatonresearch.aifh.learning.score.ScoreRegressionData;
import com.heatonresearch.aifh.normalize.DataSet;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

import java.io.InputStream;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

/**
 * Created by jeff on 11/21/15.
 */
public class LearnIrisAnneal extends SimpleLearn {
    /**
     * Run the example.
     */
    public void process() {
        try {
            final InputStream istream = this.getClass().getResourceAsStream("/iris.csv");
            if( istream==null ) {
                System.out.println("Cannot access data set, make sure the resources are available.");
                System.exit(1);
            }
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

            final ScoreFunction score = new ScoreRegressionData(trainingData);
            final TrainAnneal train = new TrainAnneal(network, score);
            performIterations(train, 100000, 0.01, true);
            queryOneOfN(network, trainingData, species);


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
