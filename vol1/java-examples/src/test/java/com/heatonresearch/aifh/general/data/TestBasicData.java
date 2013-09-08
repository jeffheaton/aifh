package com.heatonresearch.aifh.general.data;

import com.heatonresearch.aifh.AIFH;
import org.junit.Test;

import java.util.List;

import static org.junit.Assert.assertEquals;

/**
 * Test basic data.
 */
public class TestBasicData {

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
    public void testUnSupervised() {
        BasicData data = new BasicData(2);
        assertEquals(2, data.getInput().length);
        assertEquals(0, data.getIdeal().length);
    }

    @Test
    public void testSupervised() {
        BasicData data = new BasicData(2, 1);
        assertEquals(2, data.getInput().length);
        assertEquals(1, data.getIdeal().length);
    }

    @Test
    public void testLabel() {
        BasicData data = new BasicData(2);
        data.setLabel("label");
        assertEquals("label", data.getLabel());
    }

    @Test
    public void testToString() {
        BasicData data = new BasicData(2);
        assertEquals("[BasicData: input:[0.0, 0.0], ideal:[], label:null]", data.toString());
    }

    @Test
    public void testArray() {
        double[] a = {1.0, 2.0};
        BasicData d = new BasicData(a);
        assertEquals(2, d.getInput().length);
        assertEquals(0, d.getIdeal().length);

        assertEquals(1.0, d.getInput()[0], AIFH.DEFAULT_PRECISION);
        assertEquals(2.0, d.getInput()[1], AIFH.DEFAULT_PRECISION);

    }

    @Test
    public void testArrays() {
        List<BasicData> list = BasicData.convertArrays(TEST_INPUT, TEST_IDEAL);
        assertEquals(4, list.size());
        assertEquals(2, list.get(0).getInput().length);
        assertEquals(1, list.get(0).getIdeal().length);

        assertEquals(1, list.get(1).getInput()[0], AIFH.DEFAULT_PRECISION);
        assertEquals(0, list.get(1).getInput()[1], AIFH.DEFAULT_PRECISION);
        assertEquals(1, list.get(1).getIdeal()[0], AIFH.DEFAULT_PRECISION);
    }


}
