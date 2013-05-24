package com.heatonresearch.aifh.normalize;

import au.com.bytecode.opencsv.CSVReader;

import java.io.FileReader;

public class NormalizeCSVExample {

    public static void main(String[] args) {

        try {

            CSVReader reader = new CSVReader(new FileReader(args[0]));
            String[] nextLine;
            while ((nextLine = reader.readNext()) != null) {
                // nextLine[] is an array of values from the line
                System.out.println(nextLine[0] + nextLine[1] + "etc...");
            }
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }
}
