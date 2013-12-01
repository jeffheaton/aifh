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
package com.heatonresearch.aifh.examples.learning

import com.heatonresearch.aifh.learning.RegressionAlgorithm
import scala.collection.mutable.ArrayBuffer

/**
 * A simple polynomial function, of a specified order. Implemented as a regression algorithm.
 * <p/>
 * http://en.wikipedia.org/wiki/Polynomial
 *
 * Construct a polynomial function.
 *
 * @param polyOrder The order.
 */
class PolynomialFn(polyOrder: Int) extends RegressionAlgorithm {
  /**
   * The coefficients.  The first is the intercept.
   */
  override val longTermMemory = ArrayBuffer.fill(polyOrder)(0.0)

  override def computeRegression(input: Vector[Double]): Vector[Double] = {
    val x: Double = input(0)
    Vector(longTermMemory.zipWithIndex.map { case (value,idx) => value * Math.pow(x,idx.toDouble)}.sum)
  }

  override def toString: String = {
    val result: StringBuilder = new StringBuilder
    for(i <- longTermMemory.length-1 to 0 by -1) {
      val c: Double = longTermMemory(i)
      if (result.length > 0 && c >= 0) {
        result.append('+')
      }
      result.append(c)
      if (i >= 2) {
        result.append("x^")
        result.append(i)
      }
      else if (i >= 1) {
        result.append("x")
      }
    }
    result.toString()
  }
}
