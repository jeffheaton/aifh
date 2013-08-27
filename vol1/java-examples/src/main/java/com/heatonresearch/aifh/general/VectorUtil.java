package com.heatonresearch.aifh.general;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 8/27/13
 * Time: 5:28 AM
 * To change this template use File | Settings | File Templates.
 */
public final class VectorUtil {

    private VectorUtil() {

    }

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
