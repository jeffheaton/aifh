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
package com.heatonresearch.aifh.examples.error

import com.heatonresearch.aifh.error.ErrorCalculation

/**
 * Simple data holder for actual and ideal.  Just used for this example.
 * @param actual The actual data
 * @param ideal The ideal data, what the actual should have been.
 */
class DataHolder(val actual: Vector[Vector[Double]], ideal: Vector[Vector[Double]]) {

  /**
   * Calculate the error with the specified error calculation.
   *
   * @param calc The error calculation.
   * @return The error.
   */
  def calculateError(calc: ErrorCalculation): Double = {
    calc.clear()
    for(row <- 0 until actual.length) {
      calc.updateError(actual(row), ideal(row), 1.0)
    }
    calc.calculate
  }
}
