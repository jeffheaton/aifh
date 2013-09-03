package com.heatonresearch.aifh.regression;

import com.heatonresearch.aifh.AIFH;
import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.general.fns.link.LogitLinkFunction;
import org.junit.Test;

import java.util.List;

import static org.junit.Assert.assertEquals;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 9/3/13
 * Time: 6:42 AM
 * To change this template use File | Settings | File Templates.
 */
public class TestTrainReweightLeastSquares {
    @Test
    public void testTrain() {

        double[][] x = {
                {1},
                {3},
                {2},
                {200},
                {230}};

        double[][] y = {
                {1.0},
                {1.0},
                {1.0},
                {0.0},
                {0.0}
        };


        List<BasicData> trainingData = BasicData.convertArrays(x, y);
        MultipleLinearRegression regression = new MultipleLinearRegression(1);
        regression.setLinkFunction(new LogitLinkFunction());
        TrainReweightLeastSquares train = new TrainReweightLeastSquares(regression, trainingData);
        train.iteration();
        train.getError();

        double[] input = {0};
        double[] output = regression.computeRegression(input);
        assertEquals(0.6630762084733353, output[0], AIFH.DEFAULT_PRECISION);
    }
}
