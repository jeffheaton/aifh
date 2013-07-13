package com.heatonresearch.aifh.randomize;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 7/12/13
 * Time: 6:10 PM
 * To change this template use File | Settings | File Templates.
 */
public interface GenerateRandom {
    double nextGaussian();

    double nextGaussian(double high);

    double nextGaussian(double low, double high);

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
