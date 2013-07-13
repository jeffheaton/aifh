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

    boolean nextBoolean();

    long nextLong();

    double nextFloat();

    double nextDouble();

    int nextInt();

    int nextInt(int i);
}
