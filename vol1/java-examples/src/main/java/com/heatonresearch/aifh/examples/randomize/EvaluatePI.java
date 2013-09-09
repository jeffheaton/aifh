/*
 * Artificial Intelligence for Humans
 * Volume 1: Fundamental Algorithms
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh

 * Copyright 2013 by Jeff Heaton
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * For more information on Heaton Research copyrights, licenses
 * and trademarks visit:
 * http://www.heatonresearch.com/copyright
 */

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

        for (int i = 0; i < 1000000000; i++) {
            // pick a point at random.
            x = rnd.nextDouble();
            y = rnd.nextDouble();

            tries++;

            // was the point inside of a circle?
            if (x * x + y * y <= 1)
                success++;

            lastUpdate++;
            if (lastUpdate >= 1000000) {
                final double pi = 4 * (double) success / (double) tries;
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
    public static void main(final String[] args) {
        final EvaluatePI program = new EvaluatePI();
        program.process();
    }
}
