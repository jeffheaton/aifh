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
package com.heatonresearch.aifh.error

import org.scalatest.matchers.ShouldMatchers

/**
 * Utility class for error testing.
 */
object ErrorTestingUtil extends ShouldMatchers {
  def calculateError(calc: ErrorCalculation, actual: Vector[Vector[Double]], ideal: Vector[Vector[Double]]): Double = {
    calc.clear()
    calc.calculate should be (Double.PositiveInfinity plusOrMinus 0.0001)

    val actualIdealPairs = actual.zip(ideal)
    actualIdealPairs.foreach(p => calc.updateError(p._1, p._2, 1.0))

    calc.getSetSize should equal (20)
    val error1: Double = calc.calculate
    calc.clear()
    calc.calculate should be (Double.PositiveInfinity plusOrMinus 0.0001)

    for( i <- 0 until actual.length) {
      val actualData = actual(i)
      val idealData = ideal(i)
      for(j <- 0 until actualData.length) {
        calc.updateError(actualData(j), idealData(j))
      }
    }
    calc.getSetSize should equal (20)
    val error2: Double = calc.calculate
    error1 should be (error2 plusOrMinus 0.0001)
    error2
  }

  val IDEAL = Vector(Vector(1.0, 2.0, 3.0, 4.0),
    Vector(5.0, 6.0, 7.0, 8.0),
    Vector(9.0, 10.0, 11.0, 12.0),
    Vector(13.0, 14.0, 15.0, 16.0),
    Vector(17.0, 18.0, 19.0, 20.0))

  val ACTUAL = Vector(Vector(1.1, -2.0, -3.0, 4.1), Vector(-5.1, -6.0, 7.1, 8.2),
    Vector(9.1, 10.2, -11.5, 12.1), Vector(13.0, -14.0, 15.0, 16.1), Vector(17.0, 18.0, -19.0, 20.1))
}