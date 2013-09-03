package com.heatonresearch.aifh.examples.learning;

import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.learning.RBFNetwork;
import com.heatonresearch.aifh.learning.TrainGreedyRandom;
import com.heatonresearch.aifh.learning.score.ScoreFunction;
import com.heatonresearch.aifh.learning.score.ScoreRegressionData;
import com.heatonresearch.aifh.normalize.DataSet;

import java.io.InputStream;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 7/30/13
 * Time: 5:48 AM
 * To change this template use File | Settings | File Templates.
 */
public class LearnIris extends SimpleLearn {

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
            ScoreFunction score = new ScoreRegressionData(trainingData);
            TrainGreedyRandom train = new TrainGreedyRandom(true, network, score);
            performIterations(train, 1000000, 0.01, true);
            query(network, trainingData);


        } catch (Throwable t) {
            t.printStackTrace();
        }


    }

    public static void main(String[] args) {
        LearnIris prg = new LearnIris();
        prg.process();
    }
}
