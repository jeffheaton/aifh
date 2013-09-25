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
package com.heatonresearch.aifh.learning

import com.heatonresearch.aifh.AIFH
import org.scalatest.Suite
import org.scalatest.matchers.ShouldMatchers
import com.heatonresearch.aifh.randomize.BasicGenerateRandom
import com.heatonresearch.aifh.general.data.RichData
import RichData._

/**
 * Test the RBF network.
 */
class TestRBFNetwork extends Suite with ShouldMatchers {
  def testBasics() {
    val network = new RBFNetwork(2, 1, 1)

    // should be 7, (2*1) + (1+(1 bias))*1 + 3 RBF params
    // 2 + 2 + 3 = 7
    assert(7 === network.longTermMemory.length)
    assert("[RBFNetwork:inputCount=2,outputCount=1,RBFs=[[GaussianFunction:width=0.00,center=0.00,0.00]]]" === network.toString)
  }

  def testResetCompute() {
    val network = new RBFNetwork(2, 1, 1)

    val total = network.longTermMemory.sum
    total should be (0.0 plusOrMinus AIFH.DEFAULT_PRECISION)
    network.reset(new BasicGenerateRandom())

    val newTotal = network.longTermMemory.sum
    assert(Math.abs(newTotal) > AIFH.DEFAULT_PRECISION)
  }

  def testComputeRegression() {
    val network = new RBFNetwork(2, 1, 1)

    val ltm = Vector(2.0, // input 1 to RBF 1
      2.0, // input 2 to RBF 1
      5.0, // RBF width
      2.0, // RBF, center-0
      4.0, // RBF, center-1
      3.0, // RBF1 to Output 1
      4.0)  // Bias to Output 1

    network.longTermMemory.set(ltm)
    val x = Vector(1.0, 2.0)
    val y: Double = network.computeRegression(x)(0)
    // Inputs: (2*1) + (2*2) = 6
    // RBF: Gaussian(6) = 1
    // Outputs: (1*3) + (1*4) = 7
    y should be (7.0 plusOrMinus AIFH.DEFAULT_PRECISION)
  }

  def testComputeClassification() {
    val network: RBFNetwork = new RBFNetwork(2, 1, 2)
    val ltm = Vector(
      2.0,  // input 1 to RBF 1
      2.0,  // input 2 to RBF 1
      5.0,  // RBF width
      2.0,  // RBF, center-0
      4.0,  // RBF, center-1
      3.0,  // RBF1 to Output 1
      4.0,  // Bias to Output 1
      5.0,  // RBF1 to Output 2
      6.0) // Bias to Output 2
    network.longTermMemory.set(ltm)
    val x = Vector(1.0, 2.0)
    val y = network.computeRegression(x)
    // Inputs: (2*1) + (2*2) = 6
    // RBF: Gaussian(6) = 1
    // Outputs: (1*3) + (1*4) = 7
    y(0) should be (7.0 plusOrMinus AIFH.DEFAULT_PRECISION)
    // Inputs: (2*1) + (2*2) = 6
    // RBF: Gaussian(6) = 1
    // Outputs: (1*5) + (1*6) = 11
    y(1) should be (11.0 plusOrMinus AIFH.DEFAULT_PRECISION)
    val cls: Int = network.computeClassification(x)
    // class 1 is higher than class 0
    assert(1 === cls)
  }
}