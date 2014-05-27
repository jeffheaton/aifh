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
package com.heatonresearch.aifh.regression

import com.heatonresearch.aifh.AIFH
import com.heatonresearch.aifh.general.data.BasicData
import org.scalatest.Suite
import org.scalatest.matchers.ShouldMatchers

/**
 * Test Least squares.
 */
class TestTrainLeastSquares extends Suite with ShouldMatchers {
  def testTrain() {
    val x = Vector(Vector(5.0, 10.0, 2.0), Vector(10.0, 20.0, 4.0), Vector(15.0, 30.0, 6.0), Vector(20.0, 40.0, 8.0), Vector(25.0, 50.0, 10.0))
    val y = Vector(Vector(70.0), Vector(132.0), Vector(194.0), Vector(256.0), Vector(318.0))
    val trainingData = BasicData.convertArrays(x, y)
    val regression = new MultipleLinearRegression(3)
    val train = new TrainLeastSquares(regression, trainingData)
    train.iteration()
    regression.longTermMemory(0) should be (8.0 plusOrMinus 0.0001)
    regression.longTermMemory(1) should be (10.514285 plusOrMinus 0.0001)
    regression.longTermMemory(2) should be (0.14285 plusOrMinus 0.0001)
    train.getR2 should be (1.0 plusOrMinus 0.0001)
    train.getError should be (0.0 plusOrMinus AIFH.DEFAULT_PRECISION)
    for(i <- 0 until x.length) {
      val output = regression.computeRegression(x(i))
      y(i)(0) should be (output(0) plusOrMinus 0.0001)
    }
  }
}
