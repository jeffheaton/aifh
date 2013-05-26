package com.heatonresearch.aifh.normalize;

import junit.framework.TestCase;
import org.junit.Test;

import java.io.File;

public class TestDataSet extends TestCase {

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
        assertEquals(1.0, ds1.getMin(1));
        // test again, as strings are now numbers, from the last call
        assertEquals(1.0, ds1.getMin(1));
    }

    @Test
    public void testMax() {
        DataSet ds1 = generateTestData();
        assertEquals(3.0, ds1.getMax(1));
        // test again, as strings are now numbers, from the last call
        assertEquals(3.0, ds1.getMax(1));
    }

    @Test
    public void testNormalizeRange() {
        DataSet ds1 = generateTestData();
        ds1.normalizeRange(1, -1, 1);
        assertEquals(-1.0, Double.parseDouble(ds1.getData().get(0)[1].toString()));
    }

    @Test
    public void testDeNormalizeRange() {
        DataSet ds1 = generateTestData();

        double min = ds1.getMin(2);
        double max = ds1.getMax(2);

        ds1.normalizeRange(2, min, max, -1, 1);
        assertEquals(-1.0, Double.parseDouble(ds1.getData().get(0)[2].toString()));
        ds1.deNormalizeRange(2, min, max, -1, 1);
        assertEquals(0.1, Double.parseDouble(ds1.getData().get(0)[2].toString()));
    }

    @Test
    public void testNormalizeReciprocal() {
        DataSet ds1 = generateTestData();
        ds1.normalizeReciprocal(1);
        assertEquals(0.5, Double.parseDouble(ds1.getData().get(1)[1].toString()));
        ds1.deNormalizeReciprocal(1);
        assertEquals(2.0, Double.parseDouble(ds1.getData().get(1)[1].toString()));
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


}
