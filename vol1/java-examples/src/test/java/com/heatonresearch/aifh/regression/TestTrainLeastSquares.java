package com.heatonresearch.aifh.regression;

import com.heatonresearch.aifh.AIFH;
import com.heatonresearch.aifh.general.data.BasicData;
import org.junit.Test;

import java.util.List;

import static org.junit.Assert.assertEquals;

/**
 * Test Least squares.
 */
public class TestTrainLeastSquares {
    @Test
    public void testTrain() {

        double[][] x = {
                {5, 10, 2},
                {10, 20, 4},
                {15, 30, 6},
                {20, 40, 8},
                {25, 50, 10}};

        double[][] y = {
                {70},
                {132},
                {194},
                {256},
                {318}
        };


        List<BasicData> trainingData = BasicData.convertArrays(x, y);
        MultipleLinearRegression regression = new MultipleLinearRegression(3);
        TrainLeastSquares train = new TrainLeastSquares(regression, trainingData);
        train.iteration();

        assertEquals(8, regression.getLongTermMemory()[0], 0.0001);
        assertEquals(10.514285, regression.getLongTermMemory()[1], 0.0001);
        assertEquals(0.14285, regression.getLongTermMemory()[2], 0.0001);
        assertEquals(1.0, train.getR2(), 0.0001);
        assertEquals(0, train.getError(), AIFH.DEFAULT_PRECISION);

        for (int i = 0; i < x.length; i++) {
            double[] output = regression.computeRegression(x[i]);
            assertEquals(y[i][0], output[0], 0.0001);
        }
    }

}

