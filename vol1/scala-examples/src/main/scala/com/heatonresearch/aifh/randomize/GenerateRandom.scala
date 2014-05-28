/*
 * Artificial Intelligence for Humans
 * Volume 1: Fundamental Algorithms
 * Scala Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh

 * Copyright 2013 by Jeff Heaton
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
package com.heatonresearch.aifh.randomize

/**
 * Interface that defines how random numbers are generated.  Provides the means to generate both uniform and normal
 * (gaussian) distributed random numbers.
 */
trait GenerateRandom {
  /**
   * @return The next normally distributed random number.
   */
  def nextGaussian: Double

  /**
   * @return The next random boolean.
   */
  def nextBoolean: Boolean

  /**
   * @return The next random long.
   */
  def nextLong: Long

  /**
   * @return The next random floating point.
   */
  def nextFloat: Float

  /**
   * @return The next random double.
   */
  def nextDouble(): Double

  /**
   * The next random double up to a non-inclusive range.
   *
   * @param high The highest desired value.
   * @return The result.
   */
  def nextDouble(high: Double): Double

  /**
   * The next double between low (inclusive) and high (exclusive).
   *
   * @param low  The inclusive low value.
   * @param high The exclusive high value.
   * @return The result.
   */
  def nextDouble(low: Double, high: Double): Double

  /**
   * @return The next random integer.
   */
  def nextInt: Int

  /**
   * The next random int up to a non-inclusive range.
   *
   * @param high The highest desired value.
   * @return The result.
   */
  def nextInt(high: Int): Int

  /**
   * The next int between low (inclusive) and high (exclusive).
   *
   * @param low  The inclusive low value.
   * @param high The exclusive high value.
   * @return The result.
   */
  def nextInt(low: Int, high: Int): Int
}
