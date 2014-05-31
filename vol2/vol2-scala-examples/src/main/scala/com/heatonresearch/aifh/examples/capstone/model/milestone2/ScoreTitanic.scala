/*
 * Artificial Intelligence for Humans
 * Volume 2: Nature Inspired Algorithms
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh
 *
 * Copyright 2014 by Jeff Heaton
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
package com.heatonresearch.aifh.examples.capstone.model.milestone2

import com.heatonresearch.aifh.general.data.BasicData
import com.heatonresearch.aifh.learning.MLMethod
import com.heatonresearch.aifh.learning.RegressionAlgorithm
import com.heatonresearch.aifh.learning.score.ScoreFunction

/**
 * Score the Titanic model. The score is percentage cases predicted correctly.
 * @param trainingData The training data.
 */
class ScoreTitanic(val trainingData: java.util.List[BasicData]) extends ScoreFunction {

  override def calculateScore(algo: MLMethod): Double = {
    var incorrectCount: Int = 0
    var totalCount: Int = 0
    val alg: RegressionAlgorithm = algo.asInstanceOf[RegressionAlgorithm]
    import scala.collection.JavaConversions._
    for (aTrainingData <- this.trainingData) {
      totalCount += 1
      val predictSurvive: Boolean = alg.computeRegression(aTrainingData.input)(0) > 0.5
      val idealSurvive: Boolean = aTrainingData.ideal(0) > 0.5
      if (predictSurvive == idealSurvive) {
        incorrectCount += 1
      }
    }
    incorrectCount.asInstanceOf[Double] / totalCount.asInstanceOf[Double]
  }

  override def shouldMinimize: Boolean = false
}