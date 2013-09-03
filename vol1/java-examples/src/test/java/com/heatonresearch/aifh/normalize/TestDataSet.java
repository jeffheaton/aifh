package com.heatonresearch.aifh.normalize;

import com.heatonresearch.aifh.AIFH;
import com.heatonresearch.aifh.general.data.BasicData;
import org.junit.Test;

import java.io.File;
import java.util.List;

import static org.junit.Assert.*;

public class TestDataSet {

    private DataSet generateTestData() {
        String[] headers = {"text", "numeric", "dec"};
        DataSet ds = new DataSet(headers);

        Object[] row1 = {"One", "1", "0.1"};
        Object[] row2 = {"Two", "2", "0.2"};
        Object[] row3 = {"Three", "3", "0.3"};

        ds.add(row1);
        ds.add(row2);
        ds.add(row3);

        return ds;
    }

    @Test
    public void testLoadSave() {
        DataSet ds = generateTestData();

        File filename = new File("deleteme.csv");
        DataSet.save(filename, ds);
        DataSet dataset2 = DataSet.load(filename);
        filename.delete();

        assertTrue(ds.equals(dataset2));
        assertTrue(dataset2.equals(ds));

        assertEquals(3, ds.size());
        assertEquals(3, ds.getHeaderCount());
    }

    @Test
    public void testEqual() {
        DataSet ds1 = generateTestData();
        DataSet ds2 = generateTestData();
        assertTrue(ds1.equals(ds2));
    }

    @Test
    public void testNotEqualHeaders() {
        DataSet ds1 = generateTestData();
        DataSet ds2 = generateTestData();

        ds1.getHeaders()[1] = "--";

        assertFalse(ds1.equals(ds2));
    }

    @Test
    public void testNotEqualHeaderCount() {
        DataSet ds1 = generateTestData();
        DataSet ds2 = generateTestData();
        ds1.appendColumns(1);
        assertFalse(ds1.equals(ds2));
    }

    @Test
    public void testNotEqualRowCount() {
        DataSet ds1 = generateTestData();
        DataSet ds2 = generateTestData();
        ds1.getData().remove(0);
        assertFalse(ds1.equals(ds2));
    }

    @Test
    public void testNotEqualRows() {
        DataSet ds1 = generateTestData();
        DataSet ds2 = generateTestData();
        ds1.getData().get(0)[0] = "---";
        assertFalse(ds1.equals(ds2));
    }

    @Test
    public void testNotEqualOtherObject() {
        DataSet ds1 = generateTestData();
        assertFalse(ds1.equals(""));
    }

    @Test
    public void testMin() {
        DataSet ds1 = generateTestData();
        assertEquals(1.0, ds1.getMin(1), AIFH.DEFAULT_PRECISION);
        // test again, as strings are now numbers, from the last call
        assertEquals(1.0, ds1.getMin(1), AIFH.DEFAULT_PRECISION);
    }

    @Test
    public void testMax() {
        DataSet ds1 = generateTestData();
        assertEquals(3.0, ds1.getMax(1), AIFH.DEFAULT_PRECISION);
        // test again, as strings are now numbers, from the last call
        assertEquals(3.0, ds1.getMax(1), AIFH.DEFAULT_PRECISION);
    }

    @Test
    public void testNormalizeRange() {
        DataSet ds1 = generateTestData();
        ds1.normalizeRange(1, -1, 1);
        assertEquals(-1.0, Double.parseDouble(ds1.getData().get(0)[1].toString())
                , AIFH.DEFAULT_PRECISION);
    }

    @Test
    public void testDeNormalizeRange() {
        DataSet ds1 = generateTestData();

        double min = ds1.getMin(2);
        double max = ds1.getMax(2);

        ds1.normalizeRange(2, min, max, -1, 1);
        assertEquals(-1.0, Double.parseDouble(ds1.getData().get(0)[2].toString())
                , AIFH.DEFAULT_PRECISION);
        ds1.deNormalizeRange(2, min, max, -1, 1);
        assertEquals(0.1, Double.parseDouble(ds1.getData().get(0)[2].toString())
                , AIFH.DEFAULT_PRECISION);
    }

    @Test
    public void testNormalizeReciprocal() {
        DataSet ds1 = generateTestData();
        ds1.normalizeReciprocal(1);
        assertEquals(0.5, Double.parseDouble(ds1.getData().get(1)[1].toString())
                , AIFH.DEFAULT_PRECISION);
        ds1.deNormalizeReciprocal(1);
        assertEquals(2.0, Double.parseDouble(ds1.getData().get(1)[1].toString())
                , AIFH.DEFAULT_PRECISION);
    }

    @Test
    public void testEncodeNumeric() {
        DataSet ds1 = generateTestData();
        ds1.encodeNumeric(0);
    }

    @Test
    public void testEncodeOneOfN() {
        DataSet ds1 = generateTestData();
        ds1.encodeOneOfN(0);
    }

    @Test
    public void testEncodeEquilateral() {
        DataSet ds1 = generateTestData();
        ds1.encodeEquilateral(0);
    }

    @Test
    public void testDeleteColumn() {
        DataSet ds1 = generateTestData();
        ds1.deleteColumn(0);
        assertEquals(2, ds1.getHeaderCount());
        assertTrue(ds1.getHeaders()[0].equals("numeric"));
        assertTrue(ds1.getHeaders()[1].equals("dec"));
    }

    @Test
    public void testExtractUnsupervisedLabeled() {
        DataSet ds1 = generateTestData();
        List<BasicData> result = ds1.extractUnsupervisedLabeled(0);
        assertEquals(3, result.size());
        assertTrue(result.get(0).getLabel().equals("One"));
    }

    @Test
    public void testExtractSupervised() {
        DataSet ds1 = generateTestData();
        List<BasicData> result = ds1.extractSupervised(1, 1, 2, 1);
        assertEquals(3, result.size());
    }

    @Test
    public void testReplaceColumn() {
        DataSet ds1 = generateTestData();
        ds1.replaceColumn(1, 2, 1, 0);
        List<BasicData> result = ds1.extractSupervised(1, 1, 2, 1);
        assertEquals(0.0, result.get(0).getInput()[0], AIFH.DEFAULT_PRECISION);
        assertEquals(1.0, result.get(1).getInput()[0], AIFH.DEFAULT_PRECISION);
    }

    @Test
    public void testDeleteUnknowns() {
        DataSet ds1 = generateTestData();
        ds1.getData().get(1)[2] = "?";
        ds1.deleteUnknowns();
        assertEquals(2, ds1.getData().size());
    }

}
