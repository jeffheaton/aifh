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
package com.heatonresearch.aifh.examples.util

import com.heatonresearch.aifh.general.data.BasicData
import com.heatonresearch.aifh.learning.LearningMethod
import com.heatonresearch.aifh.learning.RegressionAlgorithm
import com.heatonresearch.aifh.normalize.Equilateral
import com.heatonresearch.aifh.general.VectorUtil

/**
 * Base class for many of the iteration based examples.  It will loop over iterations and display stats.
 */
object SimpleLearn {
  /**
   * Query a regression algorithm and see how close it matches the training data.
   *
   * @param alg             The algorithm to evaluate.
   * @param theTrainingData The training data.
   */
  def query(alg: RegressionAlgorithm, theTrainingData: Vector[BasicData]) {
    for (data <- theTrainingData) {
      val output = alg.computeRegression(data.input)
      println(s"${data.input} -> $output, Ideal: ${data.ideal}")
    }
  }

  /**
   * Query a regression algorithm using equilateral encoding.
   *
   * @param alg             The algorithm being used.
   * @param theTrainingData The training data.
   * @param items           The category items classified.
   * @param high            The high value.
   * @param low             The low value.
   */
  def queryEquilateral(alg: RegressionAlgorithm, theTrainingData: Seq[BasicData], items: Map[String, Int], high: Double, low: Double) {

    val invMap = items.map(kv => kv._2 -> kv._1)

    val eq = new Equilateral(items.size, high, low)
    for (data <- theTrainingData) {
      val output = alg.computeRegression(data.input)
      val idealIndex = eq.decode(data.ideal)
      val actualIndex = eq.decode(output)
      println(s"${data.input} -> ${invMap(actualIndex)}, Ideal: ${invMap(idealIndex)}")
    }
  }

  // TO DE
  def queryOneOfNOld(alg: RegressionAlgorithm, theTrainingData: java.util.List[BasicData], items: java.util.Map[String, java.lang.Integer]) {
    import scala.collection.JavaConversions._
    val x = items.asInstanceOf[java.util.Map[String,Int]]
    queryOneOfN(alg,theTrainingData.toList,x.toMap)
  }

    /**
   * Query a regression algorithm using one-of-n encoding.
   *
   * @param alg             The algorithm being used.
   * @param theTrainingData The training data.
   * @param items           The category items classified.
   */
  def queryOneOfN(alg: RegressionAlgorithm, theTrainingData: Seq[BasicData], items: Map[String, Int]) {

    val invMap = items.map(kv => kv._2 -> kv._1)

    for (data <- theTrainingData) {
      val output = alg.computeRegression(data.input)
      val idealIndex = VectorUtil.maxIndex(data.ideal)
      val actualIndex = VectorUtil.maxIndex(output)
      println(s"${data.input} -> ${invMap.get(actualIndex)}, Ideal: ${invMap.get(idealIndex)}")
    }
  }
}

class SimpleLearn {
  /**
   * Perform training iterations.
   *
   * @param train          The learning algorithm.
   * @param maxIterations  The max number of iterations.
   * @param targetScore    The target score.
   * @param shouldMinimize True, if we should minimize.
   */
  def performIterations(train: LearningMethod, maxIterations: Int, targetScore: Double, shouldMinimize: Boolean) {
    var iterationNumber: Int = 0
    var done = false
    do {
      iterationNumber += 1
      train.iteration()
      done = (train.done 
        || (iterationNumber >= maxIterations) 
        || (shouldMinimize && train.getLastError < targetScore) 
        || (!shouldMinimize && train.getLastError > targetScore))

      println(s"Iteration #$iterationNumber, Score=${train.getLastError}, ${train.getStatus}")
    } while (!done)
    train.finishTraining()
    println(s"Final score: ${train.getLastError}")
  }
}
