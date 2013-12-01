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

import com.heatonresearch.aifh.error.ErrorCalculation
import com.heatonresearch.aifh.error.ErrorCalculationMSE
import com.heatonresearch.aifh.general.data.BasicData
import com.heatonresearch.aifh.learning.MachineLearningAlgorithm
import com.heatonresearch.aifh.learning.RegressionAlgorithm

/**
 * Score regression data.  The score is done using an error calculation method.
 * @param trainingData The training data.
 */
class ScoreRegressionData(val trainingData: Vector[BasicData]) extends ScoreFunction {

  def calculateScore(algo: MachineLearningAlgorithm): Double = {
    val ralgo = algo.asInstanceOf[RegressionAlgorithm]
    errorCalc.clear()
    for (pair <- trainingData) {
      val output = ralgo.computeRegression(pair.input)
      errorCalc.updateError(output, pair.ideal, 1.0)
    }
    errorCalc.calculate
  }

  /**
   * The error calculator.
   */
  var errorCalc: ErrorCalculation = new ErrorCalculationMSE
}