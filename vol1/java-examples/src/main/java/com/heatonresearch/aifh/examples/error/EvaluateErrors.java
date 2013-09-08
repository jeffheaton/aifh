package com.heatonresearch.aifh.examples.error;

import com.heatonresearch.aifh.error.ErrorCalculation;
import com.heatonresearch.aifh.error.ErrorCalculationESS;
import com.heatonresearch.aifh.error.ErrorCalculationMSE;
import com.heatonresearch.aifh.error.ErrorCalculationRMS;
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
    public DataHolder generate(int seed, int rows, int cols, double low, double high, double distort) {
        GenerateRandom rnd = new MersenneTwisterGenerateRandom(seed);

        double[][] ideal = new double[rows][cols];
        double[][] actual = new double[rows][cols];

        for (int row = 0; row < rows; row++) {
            for (int col = 0; col < cols; col++) {
                ideal[row][col] = rnd.nextDouble(low, high);
                actual[row][col] = ideal[row][col] + (rnd.nextGaussian() * distort);
            }
        }

        DataHolder result = new DataHolder();
        result.setActual(actual);
        result.setIdeal(ideal);
        return result;
    }

    /**
     * Run the example.
     */
    public void process() {

        NumberFormat nf = NumberFormat.getInstance();

        ErrorCalculation calcESS = new ErrorCalculationESS();
        ErrorCalculation calcMSE = new ErrorCalculationMSE();
        ErrorCalculation calcRMS = new ErrorCalculationRMS();

        DataHolder smallErrors = generate(SEED, ROWS, COLS, LOW, HIGH, 0.1);
        DataHolder mediumErrors = generate(SEED, ROWS, COLS, LOW, HIGH, 0.5);
        DataHolder largeErrors = generate(SEED, ROWS, COLS, LOW, HIGH, 1.0);
        DataHolder hugeErrors = generate(SEED, ROWS, COLS, LOW, HIGH, 10.0);

        double smallESS = smallErrors.calculateError(calcESS);
        double smallMSE = smallErrors.calculateError(calcMSE);
        double smallRMS = smallErrors.calculateError(calcRMS);

        double mediumESS = mediumErrors.calculateError(calcESS);
        double mediumMSE = mediumErrors.calculateError(calcMSE);
        double mediumRMS = mediumErrors.calculateError(calcRMS);

        double largeESS = largeErrors.calculateError(calcESS);
        double largeMSE = largeErrors.calculateError(calcMSE);
        double largeRMS = largeErrors.calculateError(calcRMS);

        double hugeESS = hugeErrors.calculateError(calcESS);
        double hugeMSE = hugeErrors.calculateError(calcMSE);
        double hugeRMS = hugeErrors.calculateError(calcRMS);

        System.out.println("Type\tESS\t\t\tMSE\t\tRMS");
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
    public static void main(String[] args) {
        EvaluateErrors prg = new EvaluateErrors();
        prg.process();
    }
}
