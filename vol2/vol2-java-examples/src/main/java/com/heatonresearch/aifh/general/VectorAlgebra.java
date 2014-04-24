package com.heatonresearch.aifh.general;

import com.heatonresearch.aifh.randomize.GenerateRandom;

/**
 * Basic vector algebra operators.
 * Vectors are represented as arrays of doubles.
 *
 * This class was created to support the calculations
 * in the PSO algorithm.
 *
 * This class is thread safe.
 *
 * Contributed by:
 * Geoffroy Noel
 * https://github.com/goffer-looney
 *
 * @author Geoffroy Noel
 */
public class VectorAlgebra {
    /**
     * v1 = v1 + v2
     *
     * @param v1    an array of doubles
     * @param v2    an array of doubles
     */
    public static void add(double[] v1, double[] v2) {
        for (int i = 0; i < v1.length; i++) {
            v1[i] += v2[i];
        }
    }

    /**
     * v1 = v1 - v2
     *
     * @param v1    an array of doubles
     * @param v2    an array of doubles
     */
    public static void sub(double[] v1, double[] v2) {
        for (int i = 0; i < v1.length; i++) {
            v1[i] -= v2[i];
        }
    }

    /**
     * v = -v
     *
     * @param v     an array of doubles
     */
    public static void neg(double[] v) {
        for (int i = 0; i < v.length; i++) {
            v[i] = -v[i];
        }
    }

    /**
     * v = k * U(0,1) * v
     *
     * The components of the vector are multiplied
     * by k and a random number.
     * A new random number is generated for each
     * component.
     * Thread-safety depends on Random.nextDouble()
     *
     * @param v     an array of doubles.
     * @param k     a scalar.
     */
    public static void mulRand(GenerateRandom rnd, double[] v, double k) {
        for (int i = 0; i < v.length; i++) {
            v[i] *= k * rnd.nextDouble();
        }
    }

    /**
     * v = k * v
     *
     * The components of the vector are multiplied
     * by k.
     *
     * @param v     an array of doubles.
     * @param k     a scalar.
     */
    public static void mul(double[] v, double k) {
        for (int i = 0; i < v.length; i++) {
            v[i] *= k;
        }
    }

    /**
     * dst = src
     * Copy a vector.
     *
     * @param dst   an array of doubles
     * @param src   an array of doubles
     */
    public static void copy(double[] dst, double[] src) {
        System.arraycopy(src, 0, dst, 0, src.length);
    }

    /**
     * v = U(0, 0.1)
     *
     * @param v     an array of doubles
     */
    public static void randomise(GenerateRandom rnd,double[] v) {
        randomise(rnd, v, 0.1);
    }

    /**
     * v = U(-1, 1) * maxValue
     *
     * Randomise each component of a vector to
     * [-maxValue, maxValue].
     * thread-safety depends on Random.nextDouble().
     *
     * @param v     an array of doubles
     */
    public static void randomise(GenerateRandom rnd,double[] v, double maxValue) {
        for (int i = 0; i < v.length; i++) {
            v[i] = (2 * rnd.nextDouble() - 1) * maxValue;
        }
    }

    /**
     * For each components, reset their value to maxValue if
     * their absolute value exceeds it.
     *
     * @param v         an array of doubles
     * @param maxValue  if -1 this function does nothing
     */
    public static void clampComponents(double[] v, double maxValue) {
        if (maxValue != -1) {
            for (int i = 0; i < v.length; i++) {
                if (v[i] > maxValue) v[i] = maxValue;
                if (v[i] < -maxValue) v[i] = -maxValue;
            }
        }
    }

    /**
     * Take the dot product of two vectors.
     * @param v1 The first vector.
     * @param v2 The second vector.
     * @return The dot product.
     */
    public static double dotProduct(double[] v1, double[] v2) {
        double d = 0;
        for(int i=0;i<v1.length;i++) {
            d+=v1[i]*v2[i];
        }
        return Math.sqrt(d);
    }
}
