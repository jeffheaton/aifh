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
package com.heatonresearch.aifh.general.fns

import com.heatonresearch.aifh.AIFH
import org.scalatest.Suite
import org.scalatest.matchers.ShouldMatchers
import scala.collection.mutable.ArrayBuffer

/**
 * Test the inverse Multiquadric function.
 */
class TestInverseMultiquadricFunction extends Suite with ShouldMatchers {
  def testEvaluate() {
    val params = ArrayBuffer(5.0, 0.0, 0.0, 0.0)
    val funct = new InverseMultiquadricFunction(3, params, 0)
    val x = Vector(-1.0, 0.0, 1.0)
    val y = funct.evaluate(x)
    y should be (0.11396057645963795 plusOrMinus AIFH.DEFAULT_PRECISION)
  }

  def testToString() {
    val params = ArrayBuffer(5.0, 0.0, 0.0, 0.0)
    val funct = new InverseMultiquadricFunction(3, params, 0)
    val x = Vector(-1.0, 0.0, 1.0)
    funct.evaluate(x)
    assert(funct.toString === "[InverseMultiquadricFunction:width=5.00,center=0.00,0.00,0.00]")
  }

  def testOther() {
    val params = ArrayBuffer(5.0, 0.0, 0.0, 0.0)
    val funct = new InverseMultiquadricFunction(3, params, 0)
    assert(3 === funct.getDimensions)
    funct.setCenter(0, 100)
    funct.getCenter(0) should be (100.0 plusOrMinus AIFH.DEFAULT_PRECISION)
    funct.setWidth(5)
    funct.getWidth should be (5.0 plusOrMinus AIFH.DEFAULT_PRECISION)
  }
}