/*
 * Artificial Intelligence for Humans
 * Volume 2: Nature Inspired Algorithms
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh
 *
 * Copyright 2014 by Jeff Heaton
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
package com.heatonresearch.aifh.general

import com.heatonresearch.aifh.randomize.GenerateRandom

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
object VectorAlgebra {
  /**
   * v1 = v1 + v2
   *
   * @param v1 an array of doubles
   * @param v2 an array of doubles
   */
  def add(v1: Array[Double], v2: Array[Double]) {
    for(i <- 0 until v1.length)
      v1(i) += v2(i)
  }

  /**
   * v1 = v1 - v2
   *
   * @param v1 an array of doubles
   * @param v2 an array of doubles
   */
  def sub(v1: Array[Double], v2: Array[Double]) {
    for(i <- 0 until v1.length)
      v1(i) -= v2(i)
  }

  /**
   * v = -v
   *
   * @param v an array of doubles
   */
  def neg(v: Array[Double]) {
    for(i <- 0 until v.length)
      v(i) = -v(i)
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
  def mulRand(rnd: GenerateRandom, v: Array[Double], k: Double) {
    for(i <- 0 until v.length)
      v(i) *= k * rnd.nextDouble
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
  def mul(v: Array[Double], k: Double) {
    for(i <- 0 until v.length)
      v(i) *= k
  }

  /**
   * dst = src
   * Copy a vector.
   *
   * @param dst an array of doubles
   * @param src an array of doubles
   */
  def copy(dst: Array[Double], src: Array[Double]) {
    System.arraycopy(src, 0, dst, 0, src.length)
  }

  /**
   * v = U(0, 0.1)
   *
   * @param v an array of doubles
   */
  def randomise(rnd: GenerateRandom, v: Array[Double]) {
    randomise(rnd, v, 0.1)
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
  def randomise(rnd: GenerateRandom, v: Array[Double], maxValue: Double) {
    for(i <- 0 until v.length)
      v(i) = (2 * rnd.nextDouble - 1) * maxValue
  }

  /**
   * For each components, reset their value to maxValue if
   * their absolute value exceeds it.
   *
   * @param v        an array of doubles
   * @param maxValue if -1 this function does nothing
   */
  def clampComponents(v: Array[Double], maxValue: Double) {
    if (maxValue != -1) {
      for(i <- 0 until v.length) {
        if (v(i) > maxValue) v(i) = maxValue
        if (v(i) < -maxValue) v(i) = -maxValue
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
  def dotProduct(v1: Array[Double], v2: Array[Double]): Double = {
    var d: Double = 0
    for(i <- 0 until v1.length) {
      d += v1(i) * v2(i)
    }
    Math.sqrt(d)
  }
}