package com.heatonresearch.aifh.randomize;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 7/13/13
 * Time: 4:13 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class AbstractGenerateRandom implements GenerateRandom {

    @Override
    public int nextInt(final int low, final int high) {
        return (int) (low + (int) (nextDouble() * ((high - low))));
    }

    @Override
    public double nextDouble(final double high) {
        return nextDouble(0, high);
    }

    @Override
    public double nextDouble(final double low, final double high) {
        return (low + (int) (nextDouble() * ((high - low))));
    }

    @Override
    public int nextInt(int range) {
        return nextInt(0, range);
    }
}
