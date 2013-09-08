package com.heatonresearch.aifh.examples.randomize;

import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

/**
 * Approximate PI by Monte Carlo.
 * <p/>
 * http://en.wikipedia.org/wiki/Monte_Carlo_method
 */
public class EvaluatePI {

    /**
     * Random number generator.
     */
    private final GenerateRandom rnd = new MersenneTwisterGenerateRandom();

    public void process() {
        long tries = 0;
        int success = 0;
        int lastUpdate = 0;

        double x, y;

        for (int i = 0; i < 1000000; i++) {
            // pick a point at random.
            x = rnd.nextDouble();
            y = rnd.nextDouble();

            tries++;

            // was the point inside of a circle?
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

    /**
     * The main method.
     *
     * @param args Not used.
     */
    public static void main(String[] args) {
        EvaluatePI program = new EvaluatePI();
        program.process();
    }
}
