package com.heatonresearch.aifh.ann.general.data;

import com.heatonresearch.aifh.AIFH;
import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.general.data.TimeSeriesUtil;
import org.junit.Assert;
import org.junit.Test;

import java.util.List;

public class TestTimeSeriesUtil {

    @Test
    public void testSingleDimension() {
        double[][] dataset = {
                new double[] { 1 },
                new double[] { 2 },
                new double[] { 3 },
                new double[] { 4 },
                new double[] { 5 },
                new double[] { 6 },
                new double[] { 7 },
                new double[] { 8 },
                new double[] { 9 },
                new double[] { 10 }
        };

        List<BasicData> result = TimeSeriesUtil.slidingWindow(dataset,3,1,new int[]{0},new int[]{0});

        Assert.assertEquals(7,result.size());
        Assert.assertEquals(3,result.get(0).getInput().length);
        Assert.assertEquals(1,result.get(0).getIdeal().length);

        Assert.assertArrayEquals(new double[] {1,2,3},result.get(0).getInput(), AIFH.DEFAULT_PRECISION);
        Assert.assertArrayEquals(new double[] {2,3,4},result.get(1).getInput(), AIFH.DEFAULT_PRECISION);
        Assert.assertArrayEquals(new double[] {3,4,5},result.get(2).getInput(), AIFH.DEFAULT_PRECISION);
        Assert.assertArrayEquals(new double[] {4,5,6},result.get(3).getInput(), AIFH.DEFAULT_PRECISION);
        Assert.assertArrayEquals(new double[] {5,6,7},result.get(4).getInput(), AIFH.DEFAULT_PRECISION);
        Assert.assertArrayEquals(new double[] {6,7,8},result.get(5).getInput(), AIFH.DEFAULT_PRECISION);
        Assert.assertArrayEquals(new double[] {7,8,9},result.get(6).getInput(), AIFH.DEFAULT_PRECISION);

        Assert.assertEquals(4,result.get(0).getIdeal()[0],AIFH.DEFAULT_PRECISION);
        Assert.assertEquals(5,result.get(1).getIdeal()[0],AIFH.DEFAULT_PRECISION);
        Assert.assertEquals(6,result.get(2).getIdeal()[0],AIFH.DEFAULT_PRECISION);
        Assert.assertEquals(7,result.get(3).getIdeal()[0],AIFH.DEFAULT_PRECISION);
        Assert.assertEquals(8,result.get(4).getIdeal()[0],AIFH.DEFAULT_PRECISION);
        Assert.assertEquals(9,result.get(5).getIdeal()[0],AIFH.DEFAULT_PRECISION);
        Assert.assertEquals(10,result.get(6).getIdeal()[0],AIFH.DEFAULT_PRECISION);

    }

    @Test
    public void testMultipleDimension() {
        double[][] dataset = {
                new double[] { 1, 10 },
                new double[] { 2, 20 },
                new double[] { 3, 30 },
                new double[] { 4, 40 },
                new double[] { 5, 50 },
                new double[] { 6, 60 },
                new double[] { 7, 70 },
                new double[] { 8, 80 },
                new double[] { 9, 90 },
                new double[] { 10, 100 }
        };

        List<BasicData> result = TimeSeriesUtil.slidingWindow(dataset,3,1,new int[]{0,1},new int[]{0});

        Assert.assertEquals(7,result.size());
        Assert.assertEquals(6,result.get(0).getInput().length);
        Assert.assertEquals(1,result.get(0).getIdeal().length);

        Assert.assertArrayEquals(new double[] {1,10,2,20,3,30},result.get(0).getInput(), AIFH.DEFAULT_PRECISION);
        Assert.assertArrayEquals(new double[] {2,20,3,30,4,40},result.get(1).getInput(), AIFH.DEFAULT_PRECISION);
        Assert.assertArrayEquals(new double[] {3,30,4,40,5,50},result.get(2).getInput(), AIFH.DEFAULT_PRECISION);
        Assert.assertArrayEquals(new double[] {4,40,5,50,6,60},result.get(3).getInput(), AIFH.DEFAULT_PRECISION);
        Assert.assertArrayEquals(new double[] {5,50,6,60,7,70},result.get(4).getInput(), AIFH.DEFAULT_PRECISION);
        Assert.assertArrayEquals(new double[] {6,60,7,70,8,80},result.get(5).getInput(), AIFH.DEFAULT_PRECISION);
        Assert.assertArrayEquals(new double[] {7,70,8,80,9,90},result.get(6).getInput(), AIFH.DEFAULT_PRECISION);

        Assert.assertEquals(4,result.get(0).getIdeal()[0],AIFH.DEFAULT_PRECISION);
        Assert.assertEquals(5,result.get(1).getIdeal()[0],AIFH.DEFAULT_PRECISION);
        Assert.assertEquals(6,result.get(2).getIdeal()[0],AIFH.DEFAULT_PRECISION);
        Assert.assertEquals(7,result.get(3).getIdeal()[0],AIFH.DEFAULT_PRECISION);
        Assert.assertEquals(8,result.get(4).getIdeal()[0],AIFH.DEFAULT_PRECISION);
        Assert.assertEquals(9,result.get(5).getIdeal()[0],AIFH.DEFAULT_PRECISION);
        Assert.assertEquals(10,result.get(6).getIdeal()[0],AIFH.DEFAULT_PRECISION);

    }
}
