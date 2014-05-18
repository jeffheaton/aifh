package com.heatonresearch.aifh.examples.util;

import java.util.Locale;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 5/18/14
 * Time: 7:37 AM
 * To change this template use File | Settings | File Templates.
 */
public class FormatNumeric {
    public static String formatDouble(double d, int places) {
        String fmt = "%."+places;
        return String.format(Locale.ENGLISH, "%.2f", d);
    }
}
