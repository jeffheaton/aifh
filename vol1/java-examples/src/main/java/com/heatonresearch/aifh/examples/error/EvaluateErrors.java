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

package com.heatonresearch.aifh.examples.error;

import com.heatonresearch.aifh.error.ErrorCalculation;
import com.heatonresearch.aifh.error.ErrorCalculationMSE;
import com.heatonresearch.aifh.error.ErrorCalculationRMS;
import com.heatonresearch.aifh.error.ErrorCalculationSSE;
import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

import java.text.NumberFormat;

/**
 * Example that demonstrates how to calculate errors.  This allows you to see how different types of distortion affect
 * the final error for various error calculation methods.
 * <p/>
 * Type     ESS			MSE		RMS
 * Small	1252		0.01	0.1
 * Medium	31317		0.251	0.501
 * Large	125269		1.002	1.001
 * Huge	    12526940	100.216	10.011
 */
public class EvaluateErrors {
    /**
     * The random seed to use.
     */
    public static final int SEED = 1420;

    /**
     * The number of rows.
     */
    public static final int ROWS = 10000;

    /**
     * The number of columns.
     */
    public static final int COLS = 25;

    /**
     * The low value.
     */
    public static final double LOW = -1;

    /**
     * The high value.
     */
    public static final double HIGH = 1;

    /**
     * Generate random data.
     *
     * @param seed    The seed to use.
     * @param rows    The number of rows to generate.
     * @param cols    The number of columns to generate.
     * @param low     The low value.
     * @param high    The high value.
     * @param distort The distortion factor.
     * @return The data set.
     */
    public DataHolder generate(final int seed, final int rows, final int cols, final double low, final double high, final double distort) {
        final GenerateRandom rnd = new MersenneTwisterGenerateRandom(seed);

        final double[][] ideal = new double[rows][cols];
        final double[][] actual = new double[rows][cols];

        for (int row = 0; row < rows; row++) {
            for (int col = 0; col < cols; col++) {
                ideal[row][col] = rnd.nextDouble(low, high);
                actual[row][col] = ideal[row][col] + (rnd.nextGaussian() * distort);
            }
        }

        final DataHolder result = new DataHolder();
        result.setActual(actual);
        result.setIdeal(ideal);
        return result;
    }

    /**
     * Run the example.
     */
    public void process() {

        final NumberFormat nf = NumberFormat.getInstance();

        final ErrorCalculation calcESS = new ErrorCalculationSSE();
        final ErrorCalculation calcMSE = new ErrorCalculationMSE();
        final ErrorCalculation calcRMS = new ErrorCalculationRMS();

        final DataHolder smallErrors = generate(SEED, ROWS, COLS, LOW, HIGH, 0.1);
        final DataHolder mediumErrors = generate(SEED, ROWS, COLS, LOW, HIGH, 0.5);
        final DataHolder largeErrors = generate(SEED, ROWS, COLS, LOW, HIGH, 1.0);
        final DataHolder hugeErrors = generate(SEED, ROWS, COLS, LOW, HIGH, 10.0);

        final double smallESS = smallErrors.calculateError(calcESS);
        final double smallMSE = smallErrors.calculateError(calcMSE);
        final double smallRMS = smallErrors.calculateError(calcRMS);

        final double mediumESS = mediumErrors.calculateError(calcESS);
        final double mediumMSE = mediumErrors.calculateError(calcMSE);
        final double mediumRMS = mediumErrors.calculateError(calcRMS);

        final double largeESS = largeErrors.calculateError(calcESS);
        final double largeMSE = largeErrors.calculateError(calcMSE);
        final double largeRMS = largeErrors.calculateError(calcRMS);

        final double hugeESS = hugeErrors.calculateError(calcESS);
        final double hugeMSE = hugeErrors.calculateError(calcMSE);
        final double hugeRMS = hugeErrors.calculateError(calcRMS);

        System.out.println("Type\tSSE\t\t\tMSE\t\tRMS");
        System.out.println("Small\t" + (int) smallESS + "\t\t" + nf.format(smallMSE) + "\t" + nf.format(smallRMS));
        System.out.println("Medium\t" + (int) mediumESS + "\t\t" + nf.format(mediumMSE) + "\t" + nf.format(mediumRMS));
        System.out.println("Large\t" + (int) largeESS + "\t\t" + nf.format(largeMSE) + "\t" + nf.format(largeRMS));
        System.out.println("Huge\t" + (int) hugeESS + "\t" + nf.format(hugeMSE) + "\t" + nf.format(hugeRMS));

    }

    /**
     * The main method.
     *
     * @param args Not used.
     */
    public static void main(final String[] args) {
        final EvaluateErrors prg = new EvaluateErrors();
        prg.process();
    }
}
