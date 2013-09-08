package com.heatonresearch.aifh.learning.score;

import com.heatonresearch.aifh.AIFH;
import com.heatonresearch.aifh.error.ErrorCalculation;
import com.heatonresearch.aifh.error.ErrorCalculationMSE;
import com.heatonresearch.aifh.general.data.BasicData;
import org.junit.Test;

import java.util.List;

import static org.junit.Assert.assertEquals;

/**
 * Test regression data.
 */
public class TestScoreRegressionData {

    public static final double[][] TEST_INPUT = {
            {0.0, 0.0},
            {1.0, 0.0},
            {0.0, 1.0},
            {1.0, 1.0}
    };

    public static final double[][] TEST_IDEAL = {
            {0.0},
            {1.0},
            {1.0},
            {0.0}
    };

    @Test
    public void testGeneral() {
        List<BasicData> training = BasicData.convertArrays(TEST_INPUT, TEST_IDEAL);
        ScoreRegressionData score = new ScoreRegressionData(training);
        ErrorCalculation ec = new ErrorCalculationMSE();
        score.setErrorCalc(ec);
        assertEquals(ec, score.getErrorCalc());
    }

    @Test
    public void testRegression() {
        double[] ACTUAL = {0.0, 1.0, 0.0, 0.0};
        List<BasicData> training = BasicData.convertArrays(TEST_INPUT, TEST_IDEAL);
        ScoreRegressionData score = new ScoreRegressionData(training);
        SimpleAlgo simple = new SimpleAlgo(ACTUAL);
        double s = score.calculateScore(simple);
        assertEquals(training, score.getTrainingData());
        assertEquals(0.25, s, AIFH.DEFAULT_PRECISION);
    }
}
