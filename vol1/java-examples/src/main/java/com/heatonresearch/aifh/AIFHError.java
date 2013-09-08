package com.heatonresearch.aifh;

/**
 * General exception class.
 */
public class AIFHError extends Error {

    /**
     * Wrap a throwable.
     *
     * @param t The throable.
     */
    public AIFHError(Throwable t) {
        super(t);
    }

    /**
     * A string exception.
     *
     * @param s The string.
     */
    public AIFHError(String s) {
        super(s);
    }
}
