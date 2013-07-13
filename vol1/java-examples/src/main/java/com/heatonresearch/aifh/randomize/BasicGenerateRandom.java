package com.heatonresearch.aifh.randomize;

import java.util.Random;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 7/12/13
 * Time: 6:10 PM
 * To change this template use File | Settings | File Templates.
 */
public class BasicGenerateRandom implements GenerateRandom {
    private Random rand;

    public BasicGenerateRandom(long seed) {
        this.rand = new Random(seed);
    }

    public BasicGenerateRandom() {
        this.rand = new Random();
    }

    @Override
    public int nextInt() {
        return this.rand.nextInt();
    }

    @Override
    public int nextInt(int range) {
        return this.rand.nextInt(range);
    }

    @Override
    public double nextDouble() {
        return this.rand.nextDouble();
    }

    @Override
    public double nextFloat() {
        return this.rand.nextFloat();
    }

    @Override
    public long nextLong() {
        return this.rand.nextLong();
    }

    @Override
    public boolean nextBoolean() {
        return this.rand.nextBoolean();
    }

    @Override
    public double nextGaussian() {
        return this.rand.nextGaussian();
    }
}
