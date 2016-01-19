package com.heatonresearch.aifh.util;

import java.text.NumberFormat;
import java.util.Locale;

/**
 * Some utilities for number formatting.
 */
public class NumberFormatting {

    private static NumberFormat formater = NumberFormat.getInstance(Locale.US);

    /**
     * This method is used to convert a double to a USA (et al) formatted string, regardless of where the program is run.
     * This avoids decimal comma's from interfearing with comma separators.  Whenever possible, it is bets to usethe
     * local number formatting.  But, for cases where I want a USA (et al) format, I use this.
     * @param d The number to format.
     * @return The formatted string.
     */
    public static String double2USANUmber(double d) {
        return NumberFormatting.formater.format(d);
    }
}
