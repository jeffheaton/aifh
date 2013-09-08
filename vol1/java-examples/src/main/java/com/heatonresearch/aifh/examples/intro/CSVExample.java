package com.heatonresearch.aifh.examples.intro;

import au.com.bytecode.opencsv.CSVReader;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Arrays;

/**
 * Shows how to read a CSV file.
 */
public class CSVExample {

    /**
     * Run the example.
     */
    public void run() {
        CSVReader reader = null;

        try {
            InputStream istream = this.getClass().getResourceAsStream("/iris.csv");
            reader = new CSVReader(new InputStreamReader(istream));

            String[] nextLine;
            while ((nextLine = reader.readNext()) != null) {
                System.out.println(Arrays.toString(nextLine));
            }
        } catch (IOException ex) {
            ex.printStackTrace();
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (IOException ex) {
                    ex.printStackTrace();
                }
            }
        }
    }

    /**
     * The main method.
     *
     * @param args Arg 0 is the file to read.
     */
    public static void main(String[] args) {
        CSVExample prg = new CSVExample();
        prg.run();
    }
}
