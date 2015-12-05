package com.heatonresearch.aifh.util;

/**
 * Created by jeff on 12/4/15.
 */
public class ArrayUtil {
    public static int indexOfLargest(double[] data) {
        int result = -1;

        for (int i = 0; i < data.length; i++) {
            if (result == -1 || data[i] > data[result])
                result = i;
        }

        return result;
    }
}
