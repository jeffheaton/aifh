package com.heatonresearch.aifh.examples.error;

import com.heatonresearch.aifh.error.ErrorCalculation;
import com.heatonresearch.aifh.error.ErrorCalculationESS;
import com.heatonresearch.aifh.error.ErrorCalculationMSE;
import com.heatonresearch.aifh.error.ErrorCalculationRMS;
import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

import java.text.NumberFormat;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 7/27/13
 * Time: 3:54 PM
 * To change this template use File | Settings | File Templates.
 */
public class EvaluateErrors {

    public static final int SEED = 1420;
    public static final int ROWS = 10000;
    public static final int COLS = 25;
    public static final double LOW = -1;
    public static final double HIGH = 1;


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

    public DataHolder generateDominated(int seed, int rows, int cols, double low, double high, double distort) {
        GenerateRandom rnd = new MersenneTwisterGenerateRandom(seed);

        double[][] ideal = new double[rows][cols];
        double[][] actual = new double[rows][cols];

        for (int row = 0; row < rows; row++) {
            actual[row][0] = ideal[row][0] + (rnd.nextGaussian() * distort);

            for (int col = 1; col < cols; col++) {
                ideal[row][col] = rnd.nextDouble(low, high);
                actual[row][col] = ideal[row][col];
            }
        }

        DataHolder result = new DataHolder();
        result.setActual(actual);
        result.setIdeal(ideal);
        return result;
    }

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

    public static void main(String[] args) {
        EvaluateErrors prg = new EvaluateErrors();
        prg.process();
    }
}
