/*
 * Artificial Intelligence for Humans
 * Volume 3: Deep Learning and Neural Networks
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh
 *
 * Copyright 2014-2015 by Jeff Heaton
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * For more information on Heaton Research copyrights, licenses
 * and trademarks visit:
 * http://www.heatonresearch.com/copyright
 */
package com.heatonresearch.aifh.general;

import Jama.Matrix;
import com.heatonresearch.aifh.AIFHError;
import com.heatonresearch.aifh.randomize.GenerateRandom;

/**
 * Basic vector algebra operators.
 * Vectors are represented as arrays of doubles.
 * <p/>
 * This class was created to support the calculations
 * in the PSO algorithm.
 * <p/>
 * This class is thread safe.
 * <p/>
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
     * @param v1 an array of doubles
     * @param v2 an array of doubles
     */
    public static void add(double[] v1, double[] v2) {
        for (int i = 0; i < v1.length; i++) {
            v1[i] += v2[i];
        }
    }

    /**
     * v1 = v1 - v2
     *
     * @param v1 an array of doubles
     * @param v2 an array of doubles
     */
    public static void sub(double[] v1, double[] v2) {
        for (int i = 0; i < v1.length; i++) {
            v1[i] -= v2[i];
        }
    }

    /**
     * v = -v
     *
     * @param v an array of doubles
     */
    public static void neg(double[] v) {
        for (int i = 0; i < v.length; i++) {
            v[i] = -v[i];
        }
    }

    /**
     * v = k * U(0,1) * v
     * <p/>
     * The components of the vector are multiplied
     * by k and a random number.
     * A new random number is generated for each
     * component.
     * Thread-safety depends on Random.nextDouble()
     *
     * @param v an array of doubles.
     * @param k a scalar.
     */
    public static void mulRand(GenerateRandom rnd, double[] v, double k) {
        for (int i = 0; i < v.length; i++) {
            v[i] *= k * rnd.nextDouble();
        }
    }

    /**
     * v = k * v
     * <p/>
     * The components of the vector are multiplied
     * by k.
     *
     * @param v an array of doubles.
     * @param k a scalar.
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
     * @param dst an array of doubles
     * @param src an array of doubles
     */
    public static void copy(double[] dst, double[] src) {
        System.arraycopy(src, 0, dst, 0, src.length);
    }

    /**
     * v = U(0, 0.1)
     *
     * @param v an array of doubles
     */
    public static void randomise(GenerateRandom rnd, double[] v) {
        randomise(rnd, v, 0.1);
    }

    /**
     * v = U(-1, 1) * maxValue
     * <p/>
     * Randomise each component of a vector to
     * [-maxValue, maxValue].
     * thread-safety depends on Random.nextDouble().
     *
     * @param v an array of doubles
     */
    public static void randomise(GenerateRandom rnd, double[] v, double maxValue) {
        for (int i = 0; i < v.length; i++) {
            v[i] = (2 * rnd.nextDouble() - 1) * maxValue;
        }
    }

    /**
     * For each components, reset their value to maxValue if
     * their absolute value exceeds it.
     *
     * @param v        an array of doubles
     * @param maxValue if -1 this function does nothing
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
     *
     * @param v1 The first vector.
     * @param v2 The second vector.
     * @return The dot product.
     */
    public static double dotProduct(double[] v1, double[] v2) {
        double d = 0;
        for (int i = 0; i < v1.length; i++) {
            d += v1[i] * v2[i];
        }
        return Math.sqrt(d);
    }

    public static boolean isVector(Matrix matrix) {
        return(matrix.getRowDimension()==1 || matrix.getColumnDimension()==1);
    }

    /**
     * Compute the dot product for the two matrixes. To compute the dot product,
     * both
     *
     * @param a
     *            The first matrix.
     * @param b
     *            The second matrix.
     * @return The dot product.
     */
    public static double dotProduct(final Matrix a, final Matrix b) {
        if (!isVector(a) || !isVector(b)) {
            throw new AIFHError("To take the dot product, both matrices must be vectors.");
        }

        final double[][] aArray = a.getArray();
        final double[][] bArray = b.getArray();

        final int aLength = aArray.length == 1 ? aArray[0].length : aArray.length;
        final int bLength = bArray.length == 1 ? bArray[0].length : bArray.length;

        if (aLength != bLength) {
            throw new AIFHError("To take the dot product, both matrices must be of the same length.");
        }

        double result = 0;
        if (aArray.length == 1 && bArray.length == 1) {
            for (int i = 0; i < aLength; i++) {
                result += aArray[0][i] * bArray[0][i];
            }
        }
        else if (aArray.length == 1 && bArray[0].length == 1) {
            for (int i = 0; i < aLength; i++) {
                result += aArray[0][i] * bArray[i][0];
            }
        }
        else if (aArray[0].length == 1 && bArray.length == 1) {
            for (int i = 0; i < aLength; i++) {
                result += aArray[i][0] * bArray[0][i];
            }
        }
        else if (aArray[0].length == 1 && bArray[0].length == 1) {
            for (int i = 0; i < aLength; i++) {
                result += aArray[i][0] * bArray[i][0];
            }
        }

        return result;
    }

    /**
     * Create a row matrix.
     * @param d The vector.
     * @return A matrix.
     */
    public static Matrix createRowMatrix(double[] d) {
        Matrix result = new Matrix(1,d.length);
        for(int i=0;i<d.length;i++) {
            result.set(0,i,d[i]);
        }
        return result;
    }

    /**
     * Create a column matrix.
     * @param d The vector.
     * @return A matrix.
     */
    public static Matrix createColumnMatrix(double[] d) {
        Matrix result = new Matrix(d.length,1);
        for(int i=0;i<d.length;i++) {
            result.set(i,0,d[i]);
        }
        return result;
    }

    /**
     * Create an identity matrix.
     * @param size The size.
     * @return The matrix.
     */
    public static Matrix identityMatrix(final int size) {
        Matrix result = new Matrix(size,size);
        for(int i=0;i<size;i++) {
            result.set(i,i,1.0);
        }

        return result;
    }
}
