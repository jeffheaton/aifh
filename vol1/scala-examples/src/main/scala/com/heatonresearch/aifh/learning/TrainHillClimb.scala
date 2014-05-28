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

import com.heatonresearch.aifh.learning.score.ScoreFunction
import scala.collection.mutable.ArrayBuffer

/**
 * Train using hill climbing.  Hill climbing can be used to optimize the long term memory of a Machine Learning
 * Algorithm. This is done by moving the current long term memory values to a new location if that new location
 * gives a better score from the scoring function.
 * <p/>
 * http://en.wikipedia.org/wiki/Hill_climbing
 * Construct a hill climbing algorithm.
 *
 * @param shouldMinimize True, if we should minimize.
 * @param algorithm      The algorithm to optimize.
 * @param score          The scoring function.
 * @param acceleration      The acceleration for step sizes.
 * @param stepSizeVal          The initial step sizes.
 */
class TrainHillClimb(val shouldMinimize: Boolean, val algorithm: MachineLearningAlgorithm, val score: ScoreFunction, acceleration: Double, stepSizeVal: Double) extends LearningMethod {
  /**
   * The last result from the score function.
   * Set to a really bad value so it will be reset on the first iteration.
   */
  private var lastError: Double = if (shouldMinimize)
    Double.PositiveInfinity
      else
    Double.NegativeInfinity

  /**
   * Construct a hill climbing algorithm. Use acceleration of 1.2 and initial step size of 1.
   *
   * @param theShouldMinimize True, if we should minimize.
   * @param theAlgorithm      The algorithm to optimize.
   * @param theScore          The scoring function.
   */
  def this(theShouldMinimize: Boolean, theAlgorithm: MachineLearningAlgorithm, theScore: ScoreFunction) {
    this(theShouldMinimize, theAlgorithm, theScore, 1.2, 1)
  }


  /**
   * The candidate moves.
   */
  private val candidate = ArrayBuffer[Double](-acceleration,-1 / acceleration,0,1 / acceleration,acceleration)
  /**
   * The current step size.
   */
  private val stepSize: ArrayBuffer[Double] = ArrayBuffer.fill(algorithm.longTermMemory.size)(stepSizeVal)

  override def iteration() {
    val len: Int = algorithm.longTermMemory.length

    for(i <- 0 until len) {
      var best: Int = -1
      var bestScore: Double = if (shouldMinimize) Double.PositiveInfinity else Double.NegativeInfinity

      for(j <- 0 until candidate.length) {
        algorithm.longTermMemory(i) += stepSize(i) * candidate(j)
        val temp: Double = score.calculateScore(algorithm)
        algorithm.longTermMemory(i) -= stepSize(i) * candidate(j)

        val better = if (temp < bestScore) shouldMinimize else !shouldMinimize
        if (better) {
          bestScore = temp
          lastError = bestScore
          best = j
        }
      }
      if (best != -1) {
        algorithm.longTermMemory(i) += stepSize(i) * candidate(best)
        stepSize(i) = stepSize(i) * candidate(best)
      }
    }
  }

  override def done: Boolean = false

  override def getLastError: Double = lastError

  override def getStatus: String = ""

  override def finishTraining() {}
}
