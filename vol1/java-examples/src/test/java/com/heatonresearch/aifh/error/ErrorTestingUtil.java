package com.heatonresearch.aifh.error;

import static org.junit.Assert.assertEquals;

/**
 * Utility class for error testing.
 */
public class ErrorTestingUtil {

    public static final double[][] IDEAL = {
            {1.0, 2.0, 3.0, 4.0},
            {5.0, 6.0, 7.0, 8.0},
            {9.0, 10.0, 11.0, 12.0},
            {13.0, 14.0, 15.0, 16.0},
            {17.0, 18.0, 19.0, 20.0}
    };

    public static final double[][] ACTUAL = {
            {1.1, -2.0, -3.0, 4.1},
            {-5.1, -6.0, 7.1, 8.2},
            {9.1, 10.2, -11.5, 12.1},
            {13.0, -14.0, 15.0, 16.1},
            {17.0, 18.0, -19.0, 20.1}
    };

    public static double calculateError(ErrorCalculation calc, double[][] actual, double[][] ideal) {

        // First we are going to calculate by passing in 1d arrays to
        // the error calculation.  This is the most common case.

        calc.clear();

        assertEquals(Double.POSITIVE_INFINITY, calc.calculate(), 0.0001);

        for (int i = 0; i < actual.length; i++) {
            double[] actualData = actual[i];
            double[] idealData = ideal[i];
            calc.updateError(actualData, idealData, 1.0);
        }
        assertEquals(20, calc.getSetSize());
        double error1 = calc.calculate();

        // Secondly we are going to calculate by passing individual
        // elements.  This is less common, but the error calculation
        // should result in the same as above.

        calc.clear();

        assertEquals(Double.POSITIVE_INFINITY, calc.calculate(), 0.0001);

        for (int i = 0; i < actual.length; i++) {
            double[] actualData = actual[i];
            double[] idealData = ideal[i];
            for (int j = 0; j < actualData.length; j++) {
                calc.updateError(actualData[j], idealData[j]);
            }
        }
        assertEquals(20, calc.getSetSize());
        double error2 = calc.calculate();

        // these two should always equal
        assertEquals(error1, error2, 0.0001);


        return error2;
    }
}
