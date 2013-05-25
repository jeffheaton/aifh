package com.heatonresearch.aifh.normalize;

import au.com.bytecode.opencsv.CSVReader;

import java.io.File;
import java.io.FileReader;

public class NormalizeCSVExample {

    public static void main(String[] args) {

        if( args.length!=2 ) {
            System.out.println("Usage: NormalizeCSVExample [input file] [output file]");
            System.exit(0);
        }

        DataSet ds = DataSet.load(new File(args[0]));
        ds.normalizeRange(0,0,1);
        ds.normalizeRange(1,0,1);
        ds.normalizeRange(2,0,1);
        ds.normalizeRange(3,0,1);
        ds.encodeEquilateral(4);
        DataSet.save(new File(args[1]),ds);
    }
}
