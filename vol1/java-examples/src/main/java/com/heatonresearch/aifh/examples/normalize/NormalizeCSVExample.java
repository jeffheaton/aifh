package com.heatonresearch.aifh.examples.normalize;

import com.heatonresearch.aifh.normalize.DataSet;

import java.io.File;
import java.io.InputStream;

/**
 * A simple normalization example for the Iris data set.
 */
public class NormalizeCSVExample {
    /**
     * The main method.
     *
     * @param args Not used.
     */
    public static void main(String[] args) {
        NormalizeCSVExample prg = new NormalizeCSVExample();
        prg.run();
    }

    public void run() {
        try {
            InputStream istream = this.getClass().getResourceAsStream("/iris.csv");
            DataSet ds = DataSet.load(istream);
            istream.close();

            // The following ranges are setup for the Iris data set.  If you wish to normalize other files you will
            // need to modify the below function calls other files.
            ds.normalizeRange(0, 0, 1);
            ds.normalizeRange(1, 0, 1);
            ds.normalizeRange(2, 0, 1);
            ds.normalizeRange(3, 0, 1);
            ds.encodeEquilateral(4);
            File outputFile = new File("normalized.csv");
            DataSet.save(outputFile, ds);
            System.out.println("Output written to: " + outputFile.getAbsolutePath());
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }
}
