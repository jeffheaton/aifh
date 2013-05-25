package com.heatonresearch.aifh.normalize;

import au.com.bytecode.opencsv.CSVReader;
import au.com.bytecode.opencsv.CSVWriter;
import com.heatonresearch.aifh.AIFHError;

import java.io.*;
import java.util.*;

public class DataSet {

    private String[] headers;
    private final List<Object[]> data = new ArrayList<Object[]>();

    public DataSet(String[] theHeaders) {
        this.headers = theHeaders;
    }

    public int getHeaderCount() {
        return headers.length;
    }

    public String[] getHeaders() {
        return this.headers;
    }

    public void add(Object[] obj) {
        this.data.add(obj);
    }

    public List<Object[]> getData() {
        return this.data;
    }

    public static double convertNumeric(Object[] obj, int column) {
        double x;
        if( obj[column] instanceof Double ) {
            x = (Double) obj[column];
            obj[column] = x;
        } else {
            x = Double.parseDouble(obj[column].toString());
        }

        return x;
    }

    public double getMax(int column) {
        double result = Double.NEGATIVE_INFINITY;

        for(Object[] obj: this.data) {
            result = Math.max(result, convertNumeric(obj,column));
        }

        return result;
    }

    public double getMin(int column) {
        double result = Double.POSITIVE_INFINITY;

        for(Object[] obj: this.data) {
            result = Math.min(result, convertNumeric(obj, column));
        }

        return result;
    }

    public void normalizeRange(int column, double dataLow, double dataHigh, double normalizedLow, double normalizedHigh) {
        for(Object[] obj: this.data) {
            double x = convertNumeric(obj,column);

            obj[column] =  ((x - dataLow)
                    / (dataHigh - dataLow))
                    * (normalizedHigh - normalizedLow) + normalizedLow;
        }
    }

    public void normalizeRange(int column, double normalizedLow, double normalizedHigh) {
        double dataLow = getMin(column);
        double dataHigh = getMax(column);
        normalizeRange(column,dataLow,dataHigh,normalizedLow,normalizedHigh);
    }

    public void denormalizeRange(int column, double dataLow, double dataHigh, double normalizedLow, double normalizedHigh) {
        for(Object[] obj: this.data) {
            double x = convertNumeric(obj,column);

            obj[column] =  ((dataLow - dataHigh) * x - normalizedHigh
                    * dataLow + dataHigh * normalizedLow)
                    / (normalizedLow - normalizedHigh);
        }
    }

    public void denormalizeRange(int column, double normalizedLow, double normalizedHigh) {
        double dataLow = getMin(column);
        double dataHigh = getMax(column);
        denormalizeRange(column, dataLow, dataHigh, normalizedLow, normalizedHigh);
    }

    public void normalizeReciprocal(int column) {
        double dataLow = getMin(column);
        double dataHigh = getMax(column);

        for(Object[] obj: this.data) {
            double x = convertNumeric(obj,column);
            obj[column] =  1/x;
        }
    }

    public void denormalizeReciprocal(int column) {
        normalizeReciprocal(column);
    }

    public Map<String,Integer> enumerateClasses(int column) {
        // determine classes
        Set<String> classes = new HashSet<String>();
        for(Object[] obj: this.data) {
            classes.add(obj[column].toString());
        }
        // assign numeric values to each class
        Map<String,Integer> result = new HashMap<String,Integer>();
        int index = 0;
        for(String className:classes) {
            result.put(className,index++);
        }

        return result;
    }

    public Map<String,Integer> encodeNumeric(int column) {
        Map<String,Integer> classes = enumerateClasses(column);

        for(Object[] obj: this.data) {
            int index = classes.get(obj[column].toString());
            obj[column] = new Integer(index);
        }

        return classes;
    }

    public Map<String,Integer> encodeOneOfN(int column) {
        return encodeOneOfN(column,0,1);
    }

    public Map<String,Integer> encodeOneOfN(int column, double offValue, double onValue) {
        // remember the column name
        String name = this.headers[column];

        // make space for it
        Map<String,Integer> classes = enumerateClasses(column);
        insertColumns(column+1,classes.size()-1);

        // perform the 1 of n encode
        for(Object[] obj: this.data) {
            int index = classes.get(obj[column].toString());
            int classCount = classes.size();

            for(int i=0;i<classCount;i++) {
                obj[column+i] = (i==index)?onValue:offValue;
            }
        }

        // name the new columns
        for(int i=0;i<classes.size();i++) {
            this.headers[column+i] = name + "-" + i;
        }

        return classes;
    }

    public Map<String,Integer> encodeEquilateral(int column) {
        return encodeEquilateral(column,0,1);
    }

    public Map<String,Integer> encodeEquilateral(int column, double offValue, double onValue) {
        // remember the column name
        String name = this.headers[column];

        // make space for it
        Map<String,Integer> classes = enumerateClasses(column);
        int classCount = classes.size();
        insertColumns(column+1,classCount-1);

        // perform the equilateral
        Equilateral eq = new Equilateral(classCount,offValue, onValue);

        for(Object[] obj: this.data) {
            int index = classes.get(obj[column].toString());

            double[] encoded = eq.encode(index);

            for(int i=0;i<classCount-1;i++) {
                obj[column+i] = encoded[i];
            }
        }

        // name the new columns
        for(int i=0;i<classes.size();i++) {
            this.headers[column+i] = name + "-" + i;
        }

        return classes;
    }

    public int size() {
        return data.size();
    }

    public void appendColumns(int count) {

        // add the headers
        String[] newHeaders = new String[getHeaderCount()+count];
        for(int i=0;i<getHeaderCount();i++) {
            newHeaders[i]=this.headers[i];
        }

        for(int i=0;i<count;i++) {
            newHeaders[i+getHeaderCount()]="new";
        }

        this.headers = newHeaders;

        // add the data
        for(int rowIndex=0;rowIndex<size();rowIndex++) {
            Object[] originalRow = this.data.get(rowIndex);
            Object[] newRow = new Object[getHeaderCount()];
            for(int i=0;i<originalRow.length;i++) {
                newRow[i] = originalRow[i];
            }
            for(int i=0;i<count;i++) {
                newRow[getHeaderCount()-1-i]=Double.valueOf(0);
            }
            this.data.remove(rowIndex);
            this.data.add(rowIndex,newRow);
        }
    }

    public void insertColumns(int column, int columnCount) {
        // create space for new columns
        appendColumns(columnCount);

        // insert headers
        for(int i=getHeaderCount()-1;i>column;i--) {
            this.headers[i] = this.headers[i-columnCount];
        }

        // mark new columns headers
        for(int i=0;i<columnCount;i++) {
            this.headers[column+i] = "new";
        }

        for(Object[] obj: this.data) {
            // insert columns
            for(int i=getHeaderCount()-1;i>column;i--) {
                obj[i] = obj[i-columnCount];
            }

            // mark new columns
            for(int i=0;i<columnCount;i++) {
                obj[column+i] = Double.valueOf(0);
            }
        }



    }

    public static DataSet load(File filename) {
        try {
            FileInputStream fis = new FileInputStream(filename);
            DataSet ds = load(fis);
            fis.close();
            return ds;
        } catch(IOException ex) {
            throw (new AIFHError(ex));
        }
    }

    public static DataSet load(InputStream is) {
        DataSet result = null;

        try {
            Reader reader = new InputStreamReader(is);
            CSVReader csv = new CSVReader(reader);

            String[] headers = csv.readNext();

            result = new DataSet(headers);

            String[] nextLine;
            while ((nextLine = csv.readNext()) != null) {
                if( nextLine.length<=1 ) {
                    continue;
                } else if (nextLine.length != result.getHeaderCount()) {
                    throw new AIFHError("Found a CSV line with "
                            + nextLine.length + " columns, when expecting " + result.getHeaderCount());
                }
                Object[] obj = new Object[result.getHeaderCount()];
                for (int i = 0; i < nextLine.length; i++) {
                    obj[i] = nextLine[i];
                }
                result.add(obj);
            }
            csv.close();
        } catch (IOException ex) {
            throw (new AIFHError(ex));
        }

        return result;
    }

    public static void save(File filename, DataSet ds) {
        try {
            FileOutputStream fos = new FileOutputStream(filename);
            save(fos,ds);
            fos.close();
        } catch(IOException ex) {
            throw (new AIFHError(ex));
        }
    }

    public static void save(OutputStream os, DataSet ds) {
        try {
            Writer writer = new OutputStreamWriter(os);
            CSVWriter csv = new CSVWriter(writer);

            csv.writeNext(ds.getHeaders());
            String[] items2 = new String[ds.getHeaderCount()];

            for( Object[] item : ds.getData() ) {
                for(int i=0;i<ds.getHeaderCount();i++) {
                    items2[i] = item[i].toString();
                }
                csv.writeNext(items2);
            }
            csv.close();
        } catch (IOException ex) {
            throw new AIFHError(ex);
        }
    }


}
