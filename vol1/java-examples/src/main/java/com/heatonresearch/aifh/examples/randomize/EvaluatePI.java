package com.heatonresearch.aifh.examples.randomize;

import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 7/22/13
 * Time: 8:40 AM
 * To change this template use File | Settings | File Templates.
 */
public class EvaluatePI {

    private GenerateRandom rnd = new MersenneTwisterGenerateRandom();

    public void process() {
        long tries = 0;
        int success = 0;
        int lastUpdate = 0;

        double x, y;

        for (; ; ) {

            x = rnd.nextDouble();
            y = rnd.nextDouble();

            tries++;

            if (x * x + y * y <= 1)
                success++;

            lastUpdate++;
            if (lastUpdate >= 1000000) {
                double pi = 4 * (double) success / (double) tries;
                System.out.println("Tries=" + tries + ", pi=" + pi);
                lastUpdate = 0;
            }

        }
    }

    public static void main(String[] args) {
        EvaluatePI program = new EvaluatePI();
        program.process();
    }
}
