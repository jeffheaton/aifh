package com.heatonresearch.aifh.randomize;

import java.security.SecureRandom;
import java.util.Random;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 7/12/13
 * Time: 6:10 PM
 * To change this template use File | Settings | File Templates.
 */
public class SecureGenerateRandom extends AbstractGenerateRandom {

    private Random rand;

    public SecureGenerateRandom(long seed) {
        byte[] s = {(byte) seed};
        this.rand = new SecureRandom(s);
    }

    public SecureGenerateRandom() {
        this.rand = new SecureRandom();
    }

    @Override
    public int nextInt() {
        return this.rand.nextInt();
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
