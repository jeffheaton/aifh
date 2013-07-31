package com.heatonresearch.aifh.examples.learning;

import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.learning.TrainGreedyRandom;
import com.heatonresearch.aifh.learning.score.ScoreFunction;
import com.heatonresearch.aifh.learning.score.ScoreRegressionData;

import java.util.ArrayList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 7/30/13
 * Time: 1:59 PM
 * To change this template use File | Settings | File Templates.
 */
public class LearnPolynomial extends SimpleLearn {

    public List<BasicData> generateTrainingData() {
        List<BasicData> result = new ArrayList<BasicData>();

        for (double x = -50; x < 50; x++) {
            double y = (2 * Math.pow(x, 2)) + (4 * x) + 6;
            BasicData pair = new BasicData(1, 1);
            pair.getInput()[0] = x;
            pair.getIdeal()[0] = y;
            result.add(pair);
        }

        return result;
    }


    public void process() {
        List<BasicData> trainingData = generateTrainingData();
        PolynomialFn poly = new PolynomialFn(3);
        ScoreFunction score = new ScoreRegressionData(trainingData);
        TrainGreedyRandom train = new TrainGreedyRandom(poly, score);
        performIterations(train, 1000000, 0.01, true);
        System.out.println(poly.toString());
    }

    public static void main(String[] args) {
        LearnPolynomial prg = new LearnPolynomial();
        prg.process();
    }
}
