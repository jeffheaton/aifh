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
package com.heatonresearch.aifh.learning.score

import com.heatonresearch.aifh.AIFH
import com.heatonresearch.aifh.error.ErrorCalculation
import com.heatonresearch.aifh.error.ErrorCalculationMSE
import com.heatonresearch.aifh.general.data.BasicData
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Suite

/**
 * Test regression data.
 */
object TestScoreRegressionData {
  val TEST_INPUT = Vector(Vector(0.0, 0.0), Vector(1.0, 0.0), Vector(0.0, 1.0), Vector(1.0, 1.0))
  val TEST_IDEAL = Vector(Vector(0.0), Vector(1.0), Vector(1.0), Vector(0.0))
}

class TestScoreRegressionData extends Suite with ShouldMatchers {
  import TestScoreRegressionData._
  def testGeneral() {
    val training = BasicData.convertArrays(TEST_INPUT, TEST_IDEAL)
    val score = new ScoreRegressionData(training)
    val ec: ErrorCalculation = new ErrorCalculationMSE
    score.errorCalc = ec
    assert(ec === score.errorCalc)
  }

  def testRegression() {
    val ACTUAL = Vector(0.0, 1.0, 0.0, 0.0)
    val training = BasicData.convertArrays(TEST_INPUT, TEST_IDEAL)
    val score = new ScoreRegressionData(training)
    val simple = new SimpleAlgo(ACTUAL)
    val s = score.calculateScore(simple)
    assert(training === score.trainingData)
    s should be (0.25 plusOrMinus AIFH.DEFAULT_PRECISION)
  }
}