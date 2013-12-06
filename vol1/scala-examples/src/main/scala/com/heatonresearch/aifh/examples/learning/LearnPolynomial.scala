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
package com.heatonresearch.aifh.examples.learning

import com.heatonresearch.aifh.general.data.BasicData
import com.heatonresearch.aifh.learning.TrainGreedyRandom
import com.heatonresearch.aifh.learning.score.ScoreFunction
import com.heatonresearch.aifh.learning.score.ScoreRegressionData

/**
 * Learn a simple polynomial with the Greedy Random algorithm.
 */
object LearnPolynomial extends App {
  (new LearnPolynomial).process()
}

class LearnPolynomial extends SimpleLearn {
  def generateTrainingData: Vector[BasicData] = {
    ((-50 until 50) map { x =>
      val y = (2 * Math.pow(x, 2)) + (4 * x) + 6
      new BasicData(Vector(x.toDouble), Vector(y),null)
    }).toVector
  }

  /**
   * Run the example.
   */
  def process() {
    val trainingData = generateTrainingData
    val poly = new PolynomialFn(3)
    val scoreFn = new ScoreRegressionData(trainingData)
    val train = new TrainGreedyRandom(true, poly, scoreFn)
    performIterations(train, 1000000, 0.01, shouldMinimize = true)
    println(poly.toString)
  }
}