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
package com.heatonresearch.aifh.distance

import org.scalatest.Suite
import org.scalatest.matchers.ShouldMatchers

/**
 * Test the Euclidean distance.
 */
class TestEuclideanDistance extends Suite with ShouldMatchers {
  def testDistanceCalc() {
    val calc = new EuclideanDistance
    val pos1 = Vector(0.5, 1.0, 2.5)
    val pos2 = Vector(0.1, 2.0, -2.5)
    calc.calculate(pos1, pos2) should be (5.1146 plusOrMinus 0.001)
  }
}