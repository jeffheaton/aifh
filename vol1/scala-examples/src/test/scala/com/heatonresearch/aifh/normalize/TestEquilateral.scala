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

import com.heatonresearch.aifh.AIFH
import com.heatonresearch.aifh.AIFHError
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Suite
import com.heatonresearch.aifh.distance.{CalculateDistance, EuclideanDistance}

/**
 * Test equilateral.
 */
class TestEquilateral extends Suite with ShouldMatchers {
  def testTooFew() {
    intercept[AIFHError] {
      new Equilateral(2, -1, 1)
    }
  }

  def testEncode() {
    val eq = new Equilateral(3, -1, 1)
    val d = eq.encode(1)
    d(0) should be (0.8660254037844386 plusOrMinus AIFH.DEFAULT_PRECISION)
    d(1) should be (-0.5 plusOrMinus AIFH.DEFAULT_PRECISION)
  }

  def testDecode() {
    val eq = new Equilateral(3, -1, 1)
    val d0 = Vector(0.866, 0.5)
    val d1 = Vector(-0.866, 0.5)
    val d2 = Vector(0.0, -1.0)
    assert(2 === eq.decode(d0))
    assert(2 === eq.decode(d1))
    assert(0 === eq.decode(d2))
  }

  def testError() {
    val eq = new Equilateral(3, -1, 1)
    intercept[AIFHError] {
      eq.encode(10)
    }
  }

  /**
   * The idea of equalateral encoding is that every class is equal distant from the others.
   * This makes sure that is true.
   */
  def testAllEqual() {
    val eq = new Equilateral(10, -1, 1)
    val dc : CalculateDistance = new EuclideanDistance()
    var compareDist = 0.0

    for(x <- 0 until 10) {
      val baseClass = eq.encode(x)
      for(y <- 0 until 10) {
        if (x != y) {
          val otherClass = eq.encode(y)
          val dist = dc.calculate(baseClass, otherClass)
          if (compareDist < AIFH.DEFAULT_PRECISION) {
            compareDist = dist
          } else {
            dist should be (compareDist plusOrMinus AIFH.DEFAULT_PRECISION)
          }
        }
      }
    }
  }
}