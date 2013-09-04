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

    boolean nextBoolean();

    long nextLong();

    double nextFloat();

    double nextDouble();

    double nextDouble(double high);

    double nextDouble(double low, double high);

    int nextInt();

    int nextInt(int high);

    int nextInt(int low, int high);
}
