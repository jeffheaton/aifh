/*
 * Artificial Intelligence for Humans
 * Volume 1: Fundamental Algorithms
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh

 * Copyright 2013 by Jeff Heaton
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * For more information on Heaton Research copyrights, licenses
 * and trademarks visit:
 * http://www.heatonresearch.com/copyright
 */

package com.heatonresearch.aifh.normalize;

import com.heatonresearch.aifh.AIFH;
import com.heatonresearch.aifh.general.data.BasicData;
import org.junit.Test;

import java.io.File;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.junit.Assert.*;

/**
 * Test the data set.
 */
public class TestDataSet {

    private DataSet generateTestData() {
        final String[] headers = {"text", "numeric", "dec"};
        final DataSet ds = new DataSet(headers);

        final Object[] row1 = {"One", "1", "0.1"};
        final Object[] row2 = {"Two", "2", "0.2"};
        final Object[] row3 = {"Three", "3", "0.3"};

        ds.add(row1);
        ds.add(row2);
        ds.add(row3);

        return ds;
    }

    @Test
    public void testLoadSave() {
        final DataSet ds = generateTestData();

        final File filename = new File("deleteme.csv");
        DataSet.save(filename, ds);
        final DataSet dataset2 = DataSet.load(filename);
        assertTrue(filename.delete());

        assertTrue(ds.equals(dataset2));
        assertTrue(dataset2.equals(ds));

        assertEquals(3, ds.size());
        assertEquals(3, ds.getHeaderCount());
    }

    @Test
    public void testEqual() {
        final DataSet ds1 = generateTestData();
        final DataSet ds2 = generateTestData();
        assertTrue(ds1.equals(ds2));
    }

    @Test
    public void testNotEqualHeaders() {
        final DataSet ds1 = generateTestData();
        final DataSet ds2 = generateTestData();

        ds1.getHeaders()[1] = "--";

        assertFalse(ds1.equals(ds2));
    }

    @Test
    public void testNotEqualHeaderCount() {
        final DataSet ds1 = generateTestData();
        final DataSet ds2 = generateTestData();
        ds1.appendColumns(1);
        assertFalse(ds1.equals(ds2));
    }

    @Test
    public void testNotEqualRowCount() {
        final DataSet ds1 = generateTestData();
        final DataSet ds2 = generateTestData();
        ds1.getData().remove(0);
        assertFalse(ds1.equals(ds2));
    }

    @Test
    public void testNotEqualRows() {
        final DataSet ds1 = generateTestData();
        final DataSet ds2 = generateTestData();
        ds1.getData().get(0)[0] = "---";
        assertFalse(ds1.equals(ds2));
    }

    @Test
    public void testNotEqualOtherObject() {
        final DataSet ds1 = generateTestData();
        assertFalse(ds1.equals(""));
    }

    @Test
    public void testMin() {
        final DataSet ds1 = generateTestData();
        assertEquals(1.0, ds1.getMin(1), AIFH.DEFAULT_PRECISION);
        // test again, as strings are now numbers, from the last call
        assertEquals(1.0, ds1.getMin(1), AIFH.DEFAULT_PRECISION);
    }

    @Test
    public void testMax() {
        final DataSet ds1 = generateTestData();
        assertEquals(3.0, ds1.getMax(1), AIFH.DEFAULT_PRECISION);
        // test again, as strings are now numbers, from the last call
        assertEquals(3.0, ds1.getMax(1), AIFH.DEFAULT_PRECISION);
    }

    @Test
    public void testNormalizeRange() {
        final DataSet ds1 = generateTestData();
        ds1.normalizeRange(1, -1, 1);
        assertEquals(-1.0, Double.parseDouble(ds1.getData().get(0)[1].toString())
                , AIFH.DEFAULT_PRECISION);
    }

    @Test
    public void testDeNormalizeRange() {
        final DataSet ds1 = generateTestData();

        final double min = ds1.getMin(2);
        final double max = ds1.getMax(2);

        ds1.normalizeRange(2, min, max, -1, 1);
        assertEquals(-1.0, Double.parseDouble(ds1.getData().get(0)[2].toString())
                , AIFH.DEFAULT_PRECISION);
        ds1.deNormalizeRange(2, min, max, -1, 1);
        assertEquals(0.1, Double.parseDouble(ds1.getData().get(0)[2].toString())
                , AIFH.DEFAULT_PRECISION);
    }

    @Test
    public void testNormalizeReciprocal() {
        final DataSet ds1 = generateTestData();
        ds1.normalizeReciprocal(1);
        assertEquals(0.5, Double.parseDouble(ds1.getData().get(1)[1].toString())
                , AIFH.DEFAULT_PRECISION);
        ds1.deNormalizeReciprocal(1);
        assertEquals(2.0, Double.parseDouble(ds1.getData().get(1)[1].toString())
                , AIFH.DEFAULT_PRECISION);
    }

    @Test
    public void testEncodeNumeric() {
        final DataSet ds1 = generateTestData();
        ds1.encodeNumeric(0);
    }

    @Test
    public void testEncodeOneOfN() {
        final DataSet ds1 = generateTestData();
        ds1.encodeOneOfN(0);
    }

    @Test
    public void testEncodeEquilateral() {
        final DataSet ds1 = generateTestData();
        ds1.encodeEquilateral(0,-1,1);
        // 3 headers, first one replaced by 2 columns to store 3 values in equilateral encoding
        assertEquals(4,ds1.getHeaderCount());
        Set<Double> col1=new HashSet<>();
        Set<Double> col2=new HashSet<>();
        for (Object[] row:ds1.getData()){
            col1.add(round((Double)row[0]));
            col2.add(round((Double)row[1]));
        }
        Set<Double> expected1=new HashSet<>(Arrays.<Double>asList(0.0,-0.866,0.866));
        Set<Double> expected2=new HashSet<>(Arrays.<Double>asList(1.0,-0.5));
        assertEquals(expected1,col1);
        assertEquals(expected2,col2);

    }

    /**
     * round a double to 3 decimal places for comparisons in tests
     * @param value the value to round
     * @return the rounded value
     */
    public static double round(double value) {
        return new BigDecimal(value).setScale(3, RoundingMode.HALF_UP).doubleValue();
    }

    @Test
    public void testDeleteColumn() {
        final DataSet ds1 = generateTestData();
        ds1.deleteColumn(0);
        assertEquals(2, ds1.getHeaderCount());
        assertTrue(ds1.getHeaders()[0].equals("numeric"));
        assertTrue(ds1.getHeaders()[1].equals("dec"));
    }

    @Test
    public void testExtractUnsupervisedLabeled() {
        final DataSet ds1 = generateTestData();
        final List<BasicData> result = ds1.extractUnsupervisedLabeled(0);
        assertEquals(3, result.size());
        assertTrue(result.get(0).getLabel().equals("One"));
    }

    @Test
    public void testExtractSupervised() {
        final DataSet ds1 = generateTestData();
        final List<BasicData> result = ds1.extractSupervised(1, 1, 2, 1);
        assertEquals(3, result.size());
    }

    @Test
    public void testReplaceColumn() {
        final DataSet ds1 = generateTestData();
        ds1.replaceColumn(1, 2, 1, 0);
        final List<BasicData> result = ds1.extractSupervised(1, 1, 2, 1);
        assertEquals(0.0, result.get(0).getInput()[0], AIFH.DEFAULT_PRECISION);
        assertEquals(1.0, result.get(1).getInput()[0], AIFH.DEFAULT_PRECISION);
    }

    @Test
    public void testDeleteUnknowns() {
        final DataSet ds1 = generateTestData();
        ds1.getData().get(1)[2] = "?";
        ds1.deleteUnknowns();
        assertEquals(2, ds1.getData().size());
    }

}
