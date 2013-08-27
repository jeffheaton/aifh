package com.heatonresearch.aifh.learning.score;

import com.heatonresearch.aifh.AIFH;
import com.heatonresearch.aifh.general.data.BasicData;
import org.junit.Test;

import java.util.List;

import static org.junit.Assert.assertEquals;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 8/27/13
 * Time: 6:56 AM
 * To change this template use File | Settings | File Templates.
 */
public class TestScoreClassificationData {
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
    public void testClassification() {
        double[] ACTUAL = {0.0, 1.0, 0.0, 0.0};
        List<BasicData> training = BasicData.convertArrays(TEST_INPUT, TEST_IDEAL);
        ScoreClassificationData score = new ScoreClassificationData(training);
        SimpleAlgo simple = new SimpleAlgo(ACTUAL);
        double s = score.calculateScore(simple);
        assertEquals(0.25, s, AIFH.DEFAULT_PRECISION);
    }
}
