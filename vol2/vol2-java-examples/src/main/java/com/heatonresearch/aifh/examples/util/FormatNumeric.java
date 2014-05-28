package com.heatonresearch.aifh.examples.util;

import java.util.Locale;

/**
 * This class provides basic number formatting.
 */
public class FormatNumeric {

    /**
     * Format a double. Always use a decimal point.
     * @param d The number.
     * @param places The number of decimal places.
     * @return The formatted number.
     */
    public static String formatDouble(double d, int places) {
        String fmt = "%."+places;
        return String.format(Locale.ENGLISH, "%.2f", d);
    }
}
