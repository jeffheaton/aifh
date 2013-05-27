package com.heatonresearch.aifh;

import com.heatonresearch.aifh.intro.example.CSVExample;
import com.heatonresearch.aifh.normalize.example.NormalizeCSVExample;

public class ExampleRunner {
    public static void main(String[] args) {
        if (args.length < 1) {
            System.out.println("Usage: ExampleRunner [example class] [example arguments ...]");
            System.exit(0);
        }

        // get the example name
        String exampleName = args[0];

        // get the example args
        String[] exampleArgs = new String[args.length - 1];

        for (int i = 0; i < args.length - 1; i++) {
            exampleArgs[i] = args[i + 1];
        }

        // need to do this a better way, but for now this works
        if (exampleName.equals("NormalizeCSVExample")) {
            NormalizeCSVExample.main(exampleArgs);
        } else if (exampleName.equals("CSVExample")) {
            CSVExample.main(exampleArgs);
        }
    }
}
