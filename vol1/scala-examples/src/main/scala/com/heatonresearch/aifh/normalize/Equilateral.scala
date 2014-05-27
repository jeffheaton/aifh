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
package com.heatonresearch.aifh.normalize

import com.heatonresearch.aifh.AIFHError
import java.io.Serializable
import scala.collection.mutable.ArrayBuffer

/**
 * Used to produce an array of activations to classify data into groups. This
 * class is provided the number of groups, as well as the range that the
 * activations should fall into.
 * Guiver, John P., and Klimasauskas, Casimir, C. (1991).
 * "Applying Neural Networks, Part IV: Improving Performance." PC AI, July/August
 */
object Equilateral {
  /**
   * The minimum number of fields to use equilateral encoding.
   */
  val MIN_EQ = 3
}

/**
 * Construct an equilateral matrix.
 *
 * @param count The number of sets, these will be the rows in the matrix.
 * @param low   The high value for the outputs.
 * @param high  The low value for the outputs.
 */
class Equilateral(count: Int, low: Double, high: Double) extends Serializable {
  import Equilateral._

  if (count < MIN_EQ) {
    throw new AIFHError("Must have at least three classes.")
  }

  /**
   * The matrix of values that was generated.
   */
  private val matrix = equilat(count, low, high)

  /**
   * Decode a set of activations and see which set it has the lowest Euclidean
   * distance from.
   *
   * @param activations The output from the neural network.
   * @return The set that these activations were closest too.
   */
  final def decode(activations: Vector[Double]): Int = {
    var minValue = Double.PositiveInfinity
    var minSet: Int = -1
    for(i <- 0 until matrix.length) {
      val dist = getDistance(activations, i)
      if (dist < minValue) {
        minValue = dist
        minSet = i
      }
    }
    minSet
  }

  /**
   * Get the activations for the specified set.
   *
   * @param set The set to determine the activations for.
   * @return The activations for the specified sets.
   */
  final def encode(set: Int): Vector[Double] = {
    if (set < 0 || set > matrix.length)
      throw new AIFHError("Class out of range for equilateral: " + set)

    matrix(set)
  }

  /**
   * Called internally to generate the matrix.
   *
   * @param n    The number of sets to generate for.
   * @param low  The high end of the range of values to generate.
   * @param high The low end of the range of values to generate.
   * @return One row for each set, the columns are the activations for that
   *         set.
   */
  private def equilat(n: Int, low: Double, high: Double): Vector[Vector[Double]] = {
    var r: Double = .0
    var f: Double = .0
    val result = Vector.fill(n)(ArrayBuffer.fill(n-1)(0.0))
    result(0)(0) = -1
    result(1)(0) = 1.0
    for(k <- 2 until n) {
      // scale the matrix so far
      r = k
      f = Math.sqrt(r * r - 1.0) / r
      for(i <- 0 until k ;
          j <- 0 until (k -1))
          result(i)(j) *= f

      r = -1.0 / r
      for(i <- 0 until k) {
        result(i)(k - 1) = r
      }
      for(i <- 0 until (k-1)) {
        result(k)(i) = 0.0
      }
      result(k)(k - 1) = 1.0
    }

    // scale it
    for(row <- 0 until result.length ;
        col <- 0 until result(0).length) {
      val min = -1.0
      val max = 1.0
      result(row)(col) = ((result(row)(col) - min) / (max - min)) * (high - low) + low
    }
    result.map(_.toVector)
  }

  /**
   * Get the Euclidean distance between the specified data and the set number.
   *
   * @param data The data to check.
   * @param set  The set to check.
   * @return The distance.
   */
  final def getDistance(data: Vector[Double], set: Int): Double = {
    var result = 0.0
    for(i <- 0 until data.length) {
      result += Math.pow(data(i) - matrix(set)(i), 2)
    }
    Math.sqrt(result)
  }

}
