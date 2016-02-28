/*
 * Artificial Intelligence for Humans
 * Volume 3: Deep Learning and Neural Networks
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh
 *
 * Copyright 2014-2015 by Jeff Heaton
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

import au.com.bytecode.opencsv.CSVReader;
import au.com.bytecode.opencsv.CSVWriter;
import com.heatonresearch.aifh.AIFHError;
import org.nd4j.linalg.api.ndarray.INDArray;
import org.nd4j.linalg.dataset.DataSet;
import org.nd4j.linalg.factory.Nd4j;

import java.io.*;
import java.text.NumberFormat;
import java.text.ParseException;
import java.util.*;

/**
 * Holds a data set.  This is usually loaded from a CSV.  It can also be generated.
 */
public class NormalizeDataSet {

    /**
     * The data loaded from a CSV, or other source.
     */
    private final List<Object[]> data = new ArrayList<>();
    /**
     * The column headers.
     */
    private String[] headers;

    /**
     * The number formatter to use for this format.
     */
    private final NumberFormat numberFormatter = NumberFormat.getInstance(Locale.US);

    /**
     * Create a blank data set.
     *
     * @param theHeaders The column headers.
     */
    public NormalizeDataSet(final String[] theHeaders) {
        this.headers = theHeaders;
    }

    /**
     * Convert a column to numeric.  Save the new Double object in place of the string.
     *
     * @param obj    The column array.
     * @param column The column to change.
     * @return The numeric value.
     */
    private double convertNumeric(final Object[] obj, final int column) {
        final double x;
        if (obj[column] instanceof Double) {
            x = (Double) obj[column];
        } else {
            try {
                x = this.numberFormatter.parse(obj[column].toString()).doubleValue();
                obj[column] = x;
            } catch (ParseException e) {
                throw new AIFHError(e);
            }
        }

        return x;
    }

    /**
     * Load a CSV file from a file.
     *
     * @param filename The filename.
     * @return The data set read.
     */
    public static NormalizeDataSet load(final File filename) {
        try {
            final FileInputStream fis = new FileInputStream(filename);
            final NormalizeDataSet ds = load(fis);
            fis.close();
            return ds;
        } catch (IOException ex) {
            throw (new AIFHError(ex));
        }
    }

    /**
     * Load a CSV from an input stream.
     *
     * @param is The input stream.
     * @return The loaded file.
     */
    public static NormalizeDataSet load(final InputStream is) {
        final NormalizeDataSet result;

        try {
            final Reader reader = new InputStreamReader(is);
            final CSVReader csv = new CSVReader(reader);

            final String[] headers = csv.readNext();

            result = new NormalizeDataSet(headers);

            String[] nextLine;
            while ((nextLine = csv.readNext()) != null) {
                if (nextLine.length <= 1) {
                    continue;
                } else if (nextLine.length != result.getHeaderCount()) {
                    throw new AIFHError("Found a CSV line with "
                            + nextLine.length + " columns, when expecting " + result.getHeaderCount());
                }
                final Object[] obj = new Object[result.getHeaderCount()];
                System.arraycopy(nextLine, 0, obj, 0, nextLine.length);
                result.add(obj);
            }
            csv.close();
        } catch (IOException ex) {
            throw (new AIFHError(ex));
        }

        return result;
    }

    /**
     * Save the specified data set to a CSV file.
     *
     * @param filename The filename.
     * @param ds       The data set to save.
     */
    public static void save(final File filename, final NormalizeDataSet ds) {
        try {
            final FileOutputStream fos = new FileOutputStream(filename);
            save(fos, ds);
            fos.close();
        } catch (IOException ex) {
            throw (new AIFHError(ex));
        }
    }

    /**
     * Save the specified data to an output stream.
     *
     * @param os The output stream.
     * @param ds The data set.
     */
    public static void save(final OutputStream os, final NormalizeDataSet ds) {
        try {
            final Writer writer = new OutputStreamWriter(os);
            final CSVWriter csv = new CSVWriter(writer);

            csv.writeNext(ds.getHeaders());
            final String[] items2 = new String[ds.getHeaderCount()];

            for (final Object[] item : ds.getData()) {
                for (int i = 0; i < ds.getHeaderCount(); i++) {
                    items2[i] = item[i].toString();
                }
                csv.writeNext(items2);
            }
            csv.close();
        } catch (IOException ex) {
            throw new AIFHError(ex);
        }
    }

    /**
     * @return The number of columns (or headers).
     */
    public int getHeaderCount() {
        return this.headers.length;
    }

    /**
     * @return The column headers.
     */
    public String[] getHeaders() {
        return this.headers;
    }

    /**
     * Add a row.
     *
     * @param row The row to add.
     */
    public void add(final Object[] row) {
        this.data.add(row);
    }

    /**
     * @return The row data.
     */
    public List<Object[]> getData() {
        return this.data;
    }

    /**
     * Get the maximum numeric value for a column.
     *
     * @param column The column.
     * @return The max numeric value.
     */
    public double getMax(final int column) {
        double result = Double.NEGATIVE_INFINITY;

        for (final Object[] obj : this.data) {
            result = Math.max(result, convertNumeric(obj, column));
        }

        return result;
    }

    /**
     * Get the mean numeric value for a column.
     *
     * @param column The column.
     * @return The mean numeric value.
     */
    public double getMean(final int column) {
        double sum = 0;
        int count = 0;

        for (final Object[] obj : this.data) {
            if(!NormalizeDataSet.isMissing(obj[column].toString())) {
                sum += convertNumeric(obj, column);
                count++;
            }
        }

        return sum/count;
    }



    /**
     * Get the standard deviation value for a column.
     *
     * @param column The column.
     * @return The standard deviation numeric value.
     */
    public double getStandardDeviation(final int column) {
        double mean = getMean(column);
        double sum = 0;
        int count = 0;

        for (final Object[] obj : this.data) {
            if(!NormalizeDataSet.isMissing(obj[column].toString())) {
                double delta = mean - convertNumeric(obj, column);
                sum += delta*delta;
                count++;
            }
        }

        return Math.sqrt(sum/count);
    }

    /**
     * Determine if the specified value is missing (empty string, NULL, NA, or ?).
     * @param str The value to check.
     * @return True if missing.
     */
    public static boolean isMissing(String str) {
        return( str.equals("?") || str.trim().equals("") || str.trim().toUpperCase().equals("NA")
                || str.trim().toUpperCase().equals("NULL"));
    }

    /**
     * Get the minimum numeric value for a column.
     *
     * @param column The column.
     * @return The min numeric value.
     */
    public double getMin(final int column) {
        double result = Double.POSITIVE_INFINITY;

        for (final Object[] obj : this.data) {
            result = Math.min(result, convertNumeric(obj, column));
        }

        return result;
    }

    /**
     * Normalize a column using range normalization.
     * http://www.heatonresearch.com/wiki/Range_Normalization
     *
     * @param column         The column to normalize.
     * @param dataLow        The low value for the actual data.
     * @param dataHigh       The high value for the actual data.
     * @param normalizedLow  The desired low normalized value.
     * @param normalizedHigh The desired high normalized value.
     */
    public void normalizeRange(final int column, final double dataLow, final double dataHigh, final double normalizedLow, final double normalizedHigh) {
        for (final Object[] obj : this.data) {
            final double x = convertNumeric(obj, column);

            obj[column] = ((x - dataLow)
                    / (dataHigh - dataLow))
                    * (normalizedHigh - normalizedLow) + normalizedLow;
        }
    }

    /**
     * Normalize a column using range normalization.  Automatically determine the actual data high and low.
     * http://www.heatonresearch.com/wiki/Range_Normalization
     *
     * @param column         The column to normalize.
     * @param normalizedLow  The desired low normalized value.
     * @param normalizedHigh The desired high normalized value.
     */
    public void normalizeRange(final int column, final double normalizedLow, final double normalizedHigh) {
        final double dataLow = getMin(column);
        final double dataHigh = getMax(column);
        normalizeRange(column, dataLow, dataHigh, normalizedLow, normalizedHigh);
    }

    public void normalizeZScore(final int column) {
        final double standardDeviation =  getStandardDeviation(column);
        final double mean = getMean(column);

        for (final Object[] obj : this.data) {
            if(isMissing(obj[column].toString())) {
                obj[column] = 0; // Place at mean
            } else {
                double x = convertNumeric(obj, column);
                obj[column] = (x - mean)/standardDeviation;
            }
        }
    }

    /**
     * De-Normalize a column using range normalization.
     * http://www.heatonresearch.com/wiki/Range_Normalization
     *
     * @param column         The column to normalize.
     * @param dataLow        The low value for the actual data.
     * @param dataHigh       The high value for the actual data.
     * @param normalizedLow  The desired low normalized value.
     * @param normalizedHigh The desired high normalized value.
     */
    public void deNormalizeRange(final int column, final double dataLow, final double dataHigh, final double normalizedLow, final double normalizedHigh) {
        for (final Object[] obj : this.data) {
            final double x = convertNumeric(obj, column);

            obj[column] = ((dataLow - dataHigh) * x - normalizedHigh
                    * dataLow + dataHigh * normalizedLow)
                    / (normalizedLow - normalizedHigh);
        }
    }

    /**
     * Normalize a column using reciprocal normalization.
     * http://www.heatonresearch.com/wiki/Reciprocal_Normalization
     *
     * @param column The column to encode.
     */
    public void normalizeReciprocal(final int column) {
        for (final Object[] obj : this.data) {
            final double x = convertNumeric(obj, column);
            obj[column] = 1 / x;
        }
    }

    /**
     * De-Normalize a column using reciprocal normalization.
     * Note: normalization and de-normalization are the same mathematical operation.
     * http://www.heatonresearch.com/wiki/Reciprocal_Normalization
     *
     * @param column The column to encode.
     */
    public void deNormalizeReciprocal(final int column) {
        normalizeReciprocal(column);
    }

    /**
     * Enumerate classes (factors) into a numbered set.
     *
     * @param column The column to enumerate.
     * @return The numbered set.
     */
    public Map<String, Integer> enumerateClasses(final int column) {
        // determine classes
        final Set<String> classes = new HashSet<>();
        for (final Object[] obj : this.data) {
            classes.add(obj[column].toString());
        }
        // assign numeric values to each class
        final Map<String, Integer> result = new HashMap<>();
        int index = 0;
        for (final String className : classes) {
            result.put(className, index++);
        }

        return result;
    }

    /**
     * Encode (enumerate) a column with simple numeric index encoding.
     *
     * @param column The column to encode.
     * @return The mapping from column names to indexes.
     */
    public Map<String, Integer> encodeNumeric(final int column) {
        final Map<String, Integer> classes = enumerateClasses(column);

        for (final Object[] obj : this.data) {
            final int index = classes.get(obj[column].toString());
            obj[column] = index;
        }

        return classes;
    }

    /**
     * Encode a column using "one of n" encoding.  Use 0 for the off value, and 1 for on.
     * <p/>
     * http://www.heatonresearch.com/wiki/One_of_n
     *
     * @param column The column to use.
     * @return The column to index mapping (the same result as calling enumerateClasses).
     */
    public CategoryMap encodeOneOfN(final int column) {
        return encodeOneOfN(column, 0, 1);
    }

    /**
     * Encode a column using "one of n" encoding.
     * <p/>
     * http://www.heatonresearch.com/wiki/One_of_n
     *
     * @param column   The column to use.
     * @param offValue The off value to use.
     * @param onValue  The on value to use.
     * @return The column to index mapping (the same result as calling enumerateClasses).
     */
    public CategoryMap encodeOneOfN(final int column, final double offValue, final double onValue) {
        // remember the column name
        final String name = this.headers[column];

        // make space for it
        final Map<String, Integer> classes = enumerateClasses(column);
        insertColumns(column + 1, classes.size() - 1);

        // perform the 1 of n encode
        for (final Object[] obj : this.data) {
            final int index = classes.get(obj[column].toString());
            final int classCount = classes.size();

            for (int i = 0; i < classCount; i++) {
                obj[column + i] = (i == index) ? onValue : offValue;
            }
        }

        // name the new columns
        for (int i = 0; i < classes.size(); i++) {
            this.headers[column + i] = name + "-" + i;
        }

        return new CategoryMap(classes);
    }

    /**
     * Use equilateral encoding to encode a column, use zero for the off value and one for the on value.
     * <p/>
     * http://www.heatonresearch.com/wiki/Equilateral
     *
     * @param column The column to encode.
     * @return The column to index mapping (the same result as calling enumerateClasses).
     */
    public Map<String, Integer> encodeEquilateral(final int column) {
        return encodeEquilateral(column, 0, 1);
    }

    /**
     * Use equilateral encoding to encode a column, use zero for the off value and one for the on value.
     * <p/>
     * http://www.heatonresearch.com/wiki/Equilateral
     *
     * @param column   The column to use.
     * @param offValue The off value to use.
     * @param onValue  The on value to use.
     * @return The column to index mapping (the same result as calling enumerateClasses).
     */
    public Map<String, Integer> encodeEquilateral(final int column, final double offValue, final double onValue) {
        // remember the column name
        final String name = this.headers[column];

        // make space for it
        final Map<String, Integer> classes = enumerateClasses(column);
        final int classCount = classes.size();
        insertColumns(column + 1, classCount - 1);

        // perform the equilateral
        final Equilateral eq = new Equilateral(classCount, offValue, onValue);

        for (final Object[] obj : this.data) {
            final int index = classes.get(obj[column].toString());

            final double[] encoded = eq.encode(index);

            for (int i = 0; i < classCount - 1; i++) {
                obj[column + i] = encoded[i];
            }
        }

        // name the new columns
        for (int i = 0; i < classes.size(); i++) {
            this.headers[column + i] = name + "-" + i;
        }

        return classes;
    }

    /**
     * @return The number of rows.
     */
    public int size() {
        return this.data.size();
    }

    /**
     * Append new columns to the end of the existing columns.
     *
     * @param count The number of new columns.
     */
    public void appendColumns(final int count) {

        // add the headers
        final String[] newHeaders = new String[getHeaderCount() + count];
        System.arraycopy(this.headers, 0, newHeaders, 0, getHeaderCount());

        for (int i = 0; i < count; i++) {
            newHeaders[i + getHeaderCount()] = "new";
        }

        this.headers = newHeaders;

        // add the data
        for (int rowIndex = 0; rowIndex < size(); rowIndex++) {
            final Object[] originalRow = this.data.get(rowIndex);
            final Object[] newRow = new Object[getHeaderCount()];
            System.arraycopy(originalRow, 0, newRow, 0, originalRow.length);
            for (int i = 0; i < count; i++) {
                newRow[getHeaderCount() - 1 - i] = (double) 0;
            }
            this.data.remove(rowIndex);
            this.data.add(rowIndex, newRow);
        }
    }

    /**
     * Insert columns at a specific location.
     *
     * @param column      The column to insert BEFORE.
     * @param columnCount The count of columns to insert.
     */
    public void insertColumns(final int column, final int columnCount) {
        // create space for new columns
        appendColumns(columnCount);

        // insert headers
        System.arraycopy(this.headers, column + 1 - columnCount, this.headers, column + 1, getHeaderCount() - 1 - column);

        // mark new columns headers
        for (int i = 0; i < columnCount; i++) {
            this.headers[column + i] = "new";
        }

        for (final Object[] obj : this.data) {
            // insert columns
            System.arraycopy(obj, column + 1 - columnCount, obj, column + 1, getHeaderCount() - 1 - column);

            // mark new columns
            for (int i = 0; i < columnCount; i++) {
                obj[column + i] = (double) 0;
            }
        }


    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean equals(final Object other) {
        if (!(other instanceof NormalizeDataSet)) {
            return false;
        }

        final NormalizeDataSet otherSet = (NormalizeDataSet) other;

        // do the basic sizes match

        if (getHeaderCount() != otherSet.getHeaderCount()) {
            return false;
        }

        if (size() != otherSet.size()) {
            return false;
        }

        // do the headers match?
        for (int i = 0; i < getHeaderCount(); i++) {
            if (!this.headers[i].equals(otherSet.getHeaders()[i])) {
                return false;
            }
        }

        // does the data match?
        for (int i = 0; i < size(); i++) {
            final Object[] row1 = this.data.get(i);
            final Object[] row2 = ((NormalizeDataSet) other).getData().get(i);

            for (int j = 0; j < getHeaderCount(); j++) {
                if (!row1[j].equals(row2[j])) {
                    return false;
                }
            }
        }


        return true;
    }

    /**
     * Extract a supervised training set.  This has both input and expected (ideal) output.
     *
     * @param inputBegin The first input column.
     * @param inputCount The number of columns for input.
     * @param idealBegin The first ideal column.
     * @param idealCount The number of columns for ideal.
     * @return The training set.
     */
    public DataSet extractSupervised(final int inputBegin, final int inputCount, final int idealBegin, final int idealCount) {
        INDArray training = Nd4j.create(size(),inputCount);
        INDArray ideal = Nd4j.create(size(),idealCount);

        for (int rowIndex = 0; rowIndex < size(); rowIndex++) {
            final Object[] raw = this.data.get(rowIndex);

            for (int i = 0; i < inputCount; i++) {
                training.put(rowIndex,i, convertNumeric(raw, inputBegin + i));
            }

            for (int i = 0; i < idealCount; i++) {
                ideal.put(rowIndex,i,convertNumeric(raw, idealBegin + i));
            }
        }

        return new DataSet(training,ideal);

    }

    /**
     * Delete all rows that contain unknown data.  An unknown column has a "?" value.
     */
    public void deleteUnknowns() {
        int rowIndex = 0;
        while (rowIndex < this.data.size()) {
            final Object[] row = this.data.get(rowIndex);
            boolean remove = false;
            for (final Object aRow : row) {
                if (aRow.toString().equals("?")) {
                    remove = true;
                    break;
                }
            }

            if (remove) {
                this.data.remove(rowIndex);
            } else {
                rowIndex++;
            }
        }
    }

    /**
     * Delete the specified column.
     *
     * @param col The column to delete.
     */
    public void deleteColumn(final int col) {
        final String[] headers2 = new String[this.headers.length - 1];

        // first, remove the header
        int h2Index = 0;
        for (int i = 0; i < this.headers.length; i++) {
            if (i != col) {
                headers2[h2Index++] = this.headers[i];
            }
        }
        this.headers = headers2;

        // now process the data
        int rowIndex = 0;
        for (final Object[] row : this.data) {
            final Object[] row2 = new Object[this.headers.length];
            int r2Index = 0;
            for (int i = 0; i <= this.headers.length; i++) {
                if (i != col) {
                    row2[r2Index++] = row[i];
                }
            }
            this.data.set(rowIndex++, row2);
        }
    }

    /**
     * Replace all of the specified values in a column.
     *
     * @param columnIndex The column index.
     * @param searchFor   What to search for.
     * @param replaceWith What to replace with.
     * @param others      What to fill in the others with that do not match.
     */
    public void replaceColumn(final int columnIndex, final double searchFor, final double replaceWith, final double others) {
        for (final Object[] row : this.data) {
            final double d = convertNumeric(row, columnIndex);
            if (Math.abs(d - searchFor) < 0.0001) {
                row[columnIndex] = replaceWith;
            } else {
                row[columnIndex] = others;
            }

        }
    }

    /**
     * Extract a single column as a list of strings.
     * @param columnIndex The column to extract.
     * @return The list of strings for that column.
     */
    public List<String> columnAsList(int columnIndex) {
        List<String> result = new ArrayList<>();

        for (int rowIndex = 0; rowIndex < size(); rowIndex++) {
            final Object[] raw = this.data.get(rowIndex);
            result.add(raw[columnIndex].toString());
        }

        return result;
    }
}
