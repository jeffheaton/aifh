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
import com.heatonresearch.aifh.general.fns.link.LogitLinkFunction
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Suite

/**
 * Test reweight squares.
 */
class TestTrainReweightLeastSquares extends Suite with ShouldMatchers {
  def testTrain() {
    val x = Vector(Vector(1.0), Vector(3.0), Vector(2.0), Vector(200.0), Vector(230.0))
    val y = Vector(Vector(1.0), Vector(1.0), Vector(1.0), Vector(0.0), Vector(0.0))
    val trainingData = BasicData.convertArrays(x, y)
    val regression = new MultipleLinearRegression(1)
    regression.linkFunction = new LogitLinkFunction
    val train = new TrainReweightLeastSquares(regression, trainingData)
    train.iteration()
    train.getError
    val input = Vector(0.0)
    val output = regression.computeRegression(input)

    output(0) should be (0.8833017302699877 plusOrMinus AIFH.DEFAULT_PRECISION)
//    output(0) should be (0.6630762084733353 plusOrMinus AIFH.DEFAULT_PRECISION)
  }
}