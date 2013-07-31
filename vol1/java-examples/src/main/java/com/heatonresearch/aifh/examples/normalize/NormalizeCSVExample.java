package com.heatonresearch.aifh.examples.normalize;

import com.heatonresearch.aifh.normalize.DataSet;

import java.io.File;

public class NormalizeCSVExample {

    public static void main(String[] args) {

        if (args.length != 2) {
            System.out.println("Usage: NormalizeCSVExample [input file] [output file]");
            System.exit(0);
        }

        DataSet ds = DataSet.load(new File(args[0]));

        // The following ranges are setup for the Iris data set.  If you wish to normalize other files you will
        // need to modify the below function calls other files.
        ds.normalizeRange(0, 0, 1);
        ds.normalizeRange(1, 0, 1);
        ds.normalizeRange(2, 0, 1);
        ds.normalizeRange(3, 0, 1);
        ds.encodeEquilateral(4);
        DataSet.save(new File(args[1]), ds);
    }
}
