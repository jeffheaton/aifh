package com.heatonresearch.aifh.normalize;

import au.com.bytecode.opencsv.CSVReader;
import au.com.bytecode.opencsv.CSVWriter;
import com.heatonresearch.aifh.AIFHError;
import com.heatonresearch.aifh.general.data.BasicData;

import java.io.*;
import java.text.NumberFormat;
import java.text.ParseException;
import java.util.*;

/**
 * Holds a data set.  This is usually loaded from a CSV.  It can also be generated.
 */
public class DataSet {

    /**
     * The data loaded from a CSV, or other source.
     */
    private final List<Object[]> data = new ArrayList<Object[]>();
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
    public DataSet(String[] theHeaders) {
        this.headers = theHeaders;
    }

    /**
     * Convert a column to numeric.  Save the new Double object in place of the string.
     *
     * @param obj    The column array.
     * @param column The column to change.
     * @return The numeric value.
     */
    private double convertNumeric(Object[] obj, int column) {
        double x;
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
    public static DataSet load(File filename) {
        try {
            FileInputStream fis = new FileInputStream(filename);
            DataSet ds = load(fis);
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
    public static DataSet load(InputStream is) {
        DataSet result;

        try {
            Reader reader = new InputStreamReader(is);
            CSVReader csv = new CSVReader(reader);

            String[] headers = csv.readNext();

            result = new DataSet(headers);

            String[] nextLine;
            while ((nextLine = csv.readNext()) != null) {
                if (nextLine.length <= 1) {
                    continue;
                } else if (nextLine.length != result.getHeaderCount()) {
                    throw new AIFHError("Found a CSV line with "
                            + nextLine.length + " columns, when expecting " + result.getHeaderCount());
                }
                Object[] obj = new Object[result.getHeaderCount()];
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
    public static void save(File filename, DataSet ds) {
        try {
            FileOutputStream fos = new FileOutputStream(filename);
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
    public static void save(OutputStream os, DataSet ds) {
        try {
            Writer writer = new OutputStreamWriter(os);
            CSVWriter csv = new CSVWriter(writer);

            csv.writeNext(ds.getHeaders());
            String[] items2 = new String[ds.getHeaderCount()];

            for (Object[] item : ds.getData()) {
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
        return headers.length;
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
    public void add(Object[] row) {
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
    public double getMax(int column) {
        double result = Double.NEGATIVE_INFINITY;

        for (Object[] obj : this.data) {
            result = Math.max(result, convertNumeric(obj, column));
        }

        return result;
    }

    /**
     * Get the minimum numeric value for a column.
     *
     * @param column The column.
     * @return The min numeric value.
     */
    public double getMin(int column) {
        double result = Double.POSITIVE_INFINITY;

        for (Object[] obj : this.data) {
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
    public void normalizeRange(int column, double dataLow, double dataHigh, double normalizedLow, double normalizedHigh) {
        for (Object[] obj : this.data) {
            double x = convertNumeric(obj, column);

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
    public void normalizeRange(int column, double normalizedLow, double normalizedHigh) {
        double dataLow = getMin(column);
        double dataHigh = getMax(column);
        normalizeRange(column, dataLow, dataHigh, normalizedLow, normalizedHigh);
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
    public void deNormalizeRange(int column, double dataLow, double dataHigh, double normalizedLow, double normalizedHigh) {
        for (Object[] obj : this.data) {
            double x = convertNumeric(obj, column);

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
    public void normalizeReciprocal(int column) {
        for (Object[] obj : this.data) {
            double x = convertNumeric(obj, column);
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
    public void deNormalizeReciprocal(int column) {
        normalizeReciprocal(column);
    }

    /**
     * Enumerate classes (factors) into a numbered set.
     *
     * @param column The column to enumerate.
     * @return The numbered set.
     */
    public Map<String, Integer> enumerateClasses(int column) {
        // determine classes
        Set<String> classes = new HashSet<String>();
        for (Object[] obj : this.data) {
            classes.add(obj[column].toString());
        }
        // assign numeric values to each class
        Map<String, Integer> result = new HashMap<String, Integer>();
        int index = 0;
        for (String className : classes) {
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
    public Map<String, Integer> encodeNumeric(int column) {
        Map<String, Integer> classes = enumerateClasses(column);

        for (Object[] obj : this.data) {
            int index = classes.get(obj[column].toString());
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
    public Map<String, Integer> encodeOneOfN(int column) {
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
    public Map<String, Integer> encodeOneOfN(int column, double offValue, double onValue) {
        // remember the column name
        String name = this.headers[column];

        // make space for it
        Map<String, Integer> classes = enumerateClasses(column);
        insertColumns(column + 1, classes.size() - 1);

        // perform the 1 of n encode
        for (Object[] obj : this.data) {
            int index = classes.get(obj[column].toString());
            int classCount = classes.size();

            for (int i = 0; i < classCount; i++) {
                obj[column + i] = (i == index) ? onValue : offValue;
            }
        }

        // name the new columns
        for (int i = 0; i < classes.size(); i++) {
            this.headers[column + i] = name + "-" + i;
        }

        return classes;
    }

    /**
     * Use equilateral encoding to encode a column, use zero for the off value and one for the on value.
     * <p/>
     * http://www.heatonresearch.com/wiki/Equilateral
     *
     * @param column The column to encode.
     * @return The column to index mapping (the same result as calling enumerateClasses).
     */
    public Map<String, Integer> encodeEquilateral(int column) {
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
    public Map<String, Integer> encodeEquilateral(int column, double offValue, double onValue) {
        // remember the column name
        String name = this.headers[column];

        // make space for it
        Map<String, Integer> classes = enumerateClasses(column);
        int classCount = classes.size();
        insertColumns(column + 1, classCount - 1);

        // perform the equilateral
        Equilateral eq = new Equilateral(classCount, offValue, onValue);

        for (Object[] obj : this.data) {
            int index = classes.get(obj[column].toString());

            double[] encoded = eq.encode(index);

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
        return data.size();
    }

    /**
     * Append new columns to the end of the existing columns.
     *
     * @param count The number of new columns.
     */
    public void appendColumns(int count) {

        // add the headers
        String[] newHeaders = new String[getHeaderCount() + count];
        System.arraycopy(this.headers, 0, newHeaders, 0, getHeaderCount());

        for (int i = 0; i < count; i++) {
            newHeaders[i + getHeaderCount()] = "new";
        }

        this.headers = newHeaders;

        // add the data
        for (int rowIndex = 0; rowIndex < size(); rowIndex++) {
            Object[] originalRow = this.data.get(rowIndex);
            Object[] newRow = new Object[getHeaderCount()];
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
    public void insertColumns(int column, int columnCount) {
        // create space for new columns
        appendColumns(columnCount);

        // insert headers
        System.arraycopy(this.headers, column + 1 - columnCount, this.headers, column + 1, getHeaderCount() - 1 - column);

        // mark new columns headers
        for (int i = 0; i < columnCount; i++) {
            this.headers[column + i] = "new";
        }

        for (Object[] obj : this.data) {
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
    public boolean equals(Object other) {
        if (!(other instanceof DataSet)) {
            return false;
        }

        DataSet otherSet = (DataSet) other;

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
            Object[] row1 = this.data.get(i);
            Object[] row2 = ((DataSet) other).getData().get(i);

            for (int j = 0; j < getHeaderCount(); j++) {
                if (!row1[j].equals(row2[j])) {
                    return false;
                }
            }
        }


        return true;
    }


    public List<BasicData> extractUnsupervisedLabeled(final int labelIndex) {
        List<BasicData> result = new ArrayList<BasicData>();

        int dimensions = getHeaderCount() - 1;

        for (int rowIndex = 0; rowIndex < size(); rowIndex++) {
            Object[] raw = this.data.get(rowIndex);
            BasicData row = new BasicData(dimensions, 0, raw[labelIndex].toString());

            int colIndex = 0;
            for (int rawColIndex = 0; rawColIndex < getHeaderCount(); rawColIndex++) {
                if (rawColIndex != labelIndex) {
                    row.getInput()[colIndex++] = convertNumeric(raw, rawColIndex);
                }
            }

            result.add(row);
        }

        return result;
    }

    public List<BasicData> extractSupervised(final int inputBegin, int inputCount, int idealBegin, int idealCount) {
        List<BasicData> result = new ArrayList<BasicData>();

        for (int rowIndex = 0; rowIndex < size(); rowIndex++) {
            Object[] raw = this.data.get(rowIndex);
            BasicData row = new BasicData(inputCount, idealCount);

            for (int i = 0; i < inputCount; i++) {
                row.getInput()[i] = convertNumeric(raw, inputBegin + i);
            }

            for (int i = 0; i < idealCount; i++) {
                row.getIdeal()[i] = convertNumeric(raw, idealBegin + i);
            }

            result.add(row);
        }

        return result;

    }
}
