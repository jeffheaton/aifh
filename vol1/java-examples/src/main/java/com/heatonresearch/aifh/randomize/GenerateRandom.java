package com.heatonresearch.aifh.randomize;

/**
 * Interface that defines how random numbers are generated.  Provides the means to generate both uniform and normal
 * (gaussian) distributed random numbers.
 */
public interface GenerateRandom {

    /**
     * @return The next normally distributed random number.
     */
    double nextGaussian();

    /**
     * @return The next random boolean.
     */
    boolean nextBoolean();

    /**
     * @return The next random long.
     */
    long nextLong();

    /**
     * @return The next random floating point.
     */
    double nextFloat();

    /**
     * @return The next random double.
     */
    double nextDouble();

    /**
     * The next random double up to a non-inclusive range.
     *
     * @param high The highest desired value.
     * @return The result.
     */
    double nextDouble(double high);

    /**
     * The next double between low (inclusive) and high (exclusive).
     *
     * @param low  The inclusive low value.
     * @param high The exclusive high value.
     * @return The result.
     */
    double nextDouble(double low, double high);

    /**
     * @return The next random integer.
     */
    int nextInt();

    /**
     * The next random int up to a non-inclusive range.
     *
     * @param high The highest desired value.
     * @return The result.
     */
    int nextInt(int high);

    /**
     * The next int between low (inclusive) and high (exclusive).
     *
     * @param low  The inclusive low value.
     * @param high The exclusive high value.
     * @return The result.
     */
    int nextInt(int low, int high);
}
