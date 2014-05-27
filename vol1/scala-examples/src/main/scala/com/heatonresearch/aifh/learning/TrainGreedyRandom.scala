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
import com.heatonresearch.aifh.randomize.GenerateRandom
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom
import scala.collection.mutable.ArrayBuffer
import com.heatonresearch.aifh.general.data.RichData
import RichData._

/**
 * The Greedy Random learning algorithm is a very primitive random-walk algorithm that only takes steps that serve
 * to move the Machine Learning algorithm to a more optimal position.  This learning algorithm essentially chooses
 * random locations for the long term memory until a better set is found.
 * <p/>
 * http://en.wikipedia.org/wiki/Random_walk
 *
 * Construct a greedy random algorithm.
 *
 * @param shouldMinimize True, if we should minimize.
 * @param algorithm      The algorithm to optimize.
 * @param score          The score function.
 */
class TrainGreedyRandom(shouldMinimize: Boolean, algorithm: MachineLearningAlgorithm, score: ScoreFunction) extends LearningMethod {

  /**
   * The random number generator to use.
   */
  private val rnd: GenerateRandom = new MersenneTwisterGenerateRandom()
  /**
   * The last error.
   */
  private var lastError: Double = if(shouldMinimize) Double.PositiveInfinity else Double.NegativeInfinity
  /**
   * The low range for random number selection.
   */
  var lowRange = -10.0
  /**
   * The high range for random number selection.
   */
  var highRange = 10.0

  override def iteration() {
    // backup current state
    val oldState = algorithm.longTermMemory.clone()

    // randomize the method
    performRandomize(algorithm.longTermMemory)

    // did we improve it?  Only keep the new method if it improved (greedy).
    val currentError = score.calculateScore(algorithm)
    if (if (currentError < lastError) shouldMinimize else !shouldMinimize) {
      lastError = currentError
    }
    else
      algorithm.longTermMemory.set(oldState)
  }

  /**
   * Randomly move to a new location.  To specify a new randomization function, override this method.
   *
   * @param memory The long term memory.
   */
  def performRandomize(memory: ArrayBuffer[Double]) {
    for(i <- 0 until memory.length)
      memory(i) = rnd.nextDouble(lowRange, highRange)
  }

  override def getStatus: String = ""

  override def getLastError: Double = lastError

  override def done: Boolean = false

  override def finishTraining() {}
}
