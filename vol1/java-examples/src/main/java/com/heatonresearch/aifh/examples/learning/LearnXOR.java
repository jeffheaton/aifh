package com.heatonresearch.aifh.examples.learning;

import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.learning.RBFNetwork;
import com.heatonresearch.aifh.learning.TrainGreedyRandom;
import com.heatonresearch.aifh.learning.score.ScoreFunction;
import com.heatonresearch.aifh.learning.score.ScoreRegressionData;

import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 7/13/13
 * Time: 9:02 PM
 * To change this template use File | Settings | File Templates.
 */
public class LearnXOR extends SimpleLearn {

    /**
     * The input necessary for XOR.
     */
    public static double XOR_INPUT[][] = {{0.0, 0.0}, {1.0, 0.0},
            {0.0, 1.0}, {1.0, 1.0}};

    /**
     * The ideal data necessary for XOR.
     */
    public static double XOR_IDEAL[][] = {{0.0}, {1.0}, {1.0}, {0.0}};


    public void process() {
        List<BasicData> trainingData = BasicData.convertArrays(XOR_INPUT, XOR_IDEAL);
        RBFNetwork network = new RBFNetwork(2, 5, 1);
        ScoreFunction score = new ScoreRegressionData(trainingData);
        TrainGreedyRandom train = new TrainGreedyRandom(network, score);
        performIterations(train, 1000000, 0.01, true);
        query(network, trainingData);
    }

    public static void main(String[] args) {
        LearnXOR prg = new LearnXOR();
        prg.process();
    }
}
