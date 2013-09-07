package com.heatonresearch.aifh.general;

/**
 * Some vector utilities.
 */
public final class VectorUtil {

    /**
     * Private constructor.
     */
    private VectorUtil() {

    }

    /**
     * Return the index that has the max value.
     *
     * @param a The vector.
     * @return The index.
     */
    public static int maxIndex(double[] a) {
        int result = -1;
        double max = Double.NEGATIVE_INFINITY;

        for (int i = 0; i < a.length; i++) {
            if (a[i] > max) {
                max = a[i];
                result = i;
            }
        }

        return result;
    }
}
