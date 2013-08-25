package com.heatonresearch.aifh.examples.optimization;

import com.heatonresearch.aifh.examples.learning.SimpleLearn;
import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.learning.RBFNetwork;
import com.heatonresearch.aifh.learning.TrainNelderMead;
import com.heatonresearch.aifh.learning.score.ScoreFunction;
import com.heatonresearch.aifh.learning.score.ScoreRegressionData;
import com.heatonresearch.aifh.normalize.DataSet;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

import java.io.InputStream;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 8/24/13
 * Time: 8:23 PM
 * To change this template use File | Settings | File Templates.
 */
public class LearnIrisNelderMead extends SimpleLearn {

    public void process() {
        try {
            InputStream istream = this.getClass().getResourceAsStream("/iris.csv");

            DataSet ds = DataSet.load(istream);
            // The following ranges are setup for the Iris data set.  If you wish to normalize other files you will
            // need to modify the below function calls other files.
            ds.normalizeRange(0, 0, 1);
            ds.normalizeRange(1, 0, 1);
            ds.normalizeRange(2, 0, 1);
            ds.normalizeRange(3, 0, 1);
            ds.encodeEquilateral(4);
            istream.close();

            List<BasicData> trainingData = ds.extractSupervised(0, 4, 4, 2);

            RBFNetwork network = new RBFNetwork(4, 4, 2);
            network.reset(new MersenneTwisterGenerateRandom());
            ScoreFunction score = new ScoreRegressionData(trainingData);
            TrainNelderMead train = new TrainNelderMead(network, score);
            performIterations(train, 1000, 0.01, true);
            query(network, trainingData);


        } catch (Throwable t) {
            t.printStackTrace();
        }


    }

    public static void main(String[] args) {
        LearnIrisNelderMead prg = new LearnIrisNelderMead();
        prg.process();
    }
}
