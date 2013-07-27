package com.heatonresearch.aifh.examples.intro;

import au.com.bytecode.opencsv.CSVReader;

import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;

public class CSVExample {
    public static void main(String[] args) {

        if (args.length != 2) {
            System.out.println("Usage: CSVExample [csv file]");
            System.exit(0);
        }

        CSVReader reader = null;

        try {
            reader = new CSVReader(new FileReader(args[0]));
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
}
