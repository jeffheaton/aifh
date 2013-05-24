package com.heatonresearch.aifh.normalize;

import au.com.bytecode.opencsv.CSVReader;
import com.heatonresearch.aifh.AIFHError;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.ArrayList;
import java.util.List;

public class DataSet {

    private final String[] headers;
    private final List<Object[]> data = new ArrayList<Object[]>();

    public DataSet(String[] theHeaders) {
        this.headers = theHeaders;
    }

    public int getHeaderCount() {
        return headers.length;
    }

    public void add(Object[] obj) {
        this.data.add(obj);
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
                if (nextLine.length != result.getHeaderCount()) {
                    throw new AIFHError("Found a CSV line with "
                            + nextLine.length + " columns, when expecting " + result.getHeaderCount());
                }
                Object[] obj = new Object[result.getHeaderCount()];
                for(int i=0;i<nextLine.length;i++) {
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

    public static void save(InputStream is, DataSet ds) {

    }


}
