package com.heatonresearch.aifh.randomize;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 9/2/13
 * Time: 5:20 PM
 * To change this template use File | Settings | File Templates.
 */
public class TestSecureGenerateRandom {

    @org.junit.Test
    public void testGenerateBoolean() {
        SecureGenerateRandom rnd = new SecureGenerateRandom(1);
        rnd.nextBoolean();
    }

    @org.junit.Test
    public void testDoubleRange() {
        SecureGenerateRandom rnd = new SecureGenerateRandom(1);
        rnd.nextDouble(-1, 1);
    }

    @org.junit.Test
    public void testDouble() {
        SecureGenerateRandom rnd = new SecureGenerateRandom(1);
        rnd.nextDouble();
    }

    @org.junit.Test
    public void testLong() {
        SecureGenerateRandom rnd = new SecureGenerateRandom(1);
        rnd.nextLong();
    }

    @org.junit.Test
    public void testFloat() {
        SecureGenerateRandom rnd = new SecureGenerateRandom(1);
        rnd.nextFloat();
    }

    @org.junit.Test
    public void testGaussianFloat() {
        SecureGenerateRandom rnd = new SecureGenerateRandom(1);
        rnd.nextGaussian();
    }

    @org.junit.Test
    public void testInt() {
        SecureGenerateRandom rnd = new SecureGenerateRandom(1);
        rnd.nextInt();
    }

    @org.junit.Test
    public void testIntRange() {
        SecureGenerateRandom rnd = new SecureGenerateRandom(1);
        rnd.nextInt(0, 10);
    }
}
