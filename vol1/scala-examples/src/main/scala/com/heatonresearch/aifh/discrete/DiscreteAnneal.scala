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
package com.heatonresearch.aifh.discrete

import com.heatonresearch.aifh.randomize.GenerateRandom
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom

/**
 * Perform discrete simulated annealing.  Discrete simulated annealing involves a problem with
 * a finite number of positions (or potential solutions).
 * @param kMax                The maximum number of iterations.
 * @param startingTemperature The starting temperature.
 * @param endingTemperature   The ending temperature.
 */
abstract class DiscreteAnneal(val kMax: Int, val startingTemperature: Double, val endingTemperature: Double) {

  /**
   * @return The correct temperature for the current iteration.
   */
  def coolingSchedule: Double = {
    val ex = k.toDouble / kMax.toDouble
    startingTemperature * Math.pow(endingTemperature / startingTemperature, ex)
  }

  /**
   * Perform one training iteration.  This will execute the specified number of cycles at the current
   * temperature.
   */
  def iteration() {
    if (k == 0) {
      currentScore = evaluate
      foundNewBest()
      globalBestScore = currentScore
    }
    k += 1
    currentTemperature = coolingSchedule
    for(cycle <- 0 until cycles) {
      backupState()
      moveToNeighbor()
      val trialScore = evaluate
      var keep = false
      if (trialScore < currentScore)
        keep = true
      else {
        lastProbability = calcProbability(currentScore, trialScore, currentTemperature)
        if (lastProbability > rnd.nextDouble)
          keep = true
      }
      if (keep) {
        currentScore = trialScore
        if (trialScore < globalBestScore) {
          globalBestScore = trialScore
          foundNewBest()
        }
      }
      else
        restoreState()
    }
  }

  /**
   * Backup the current position (or state).
   */
  def backupState()

  /**
   * Restore the current position (or state).
   */
  def restoreState()

  /**
   * Handle the fact that we found a new global best.
   */
  def foundNewBest()

  /**
   * Move to a neighbor position.
   */
  def moveToNeighbor()

  /**
   * Evaluate the current position.
   *
   * @return The score.
   */
  def evaluate: Double

  /**
   * @return True, if training has reached the last iteration.
   */
  def done: Boolean = k >= kMax

  /**
   * @return The best score found so far.
   */
  def getBestScore: Double = globalBestScore

  /**
   * Calculate the probability that a worse solution will be accepted.  The higher the temperature the more likely
   * this will happen.
   *
   * @param ecurrent The current energy (or score/error).
   * @param enew     The new energy (or score/error).
   * @param t        The current temperature.
   * @return The probability of accepting a worse solution.
   */
  def calcProbability(ecurrent: Double, enew: Double, t: Double): Double = {
    Math.exp(-(Math.abs(enew - ecurrent) / t))
  }

  /**
   * @return The current iteration.
   */
  def getK: Int = k

  /**
   * @return Returns the current status of the algorithm.
   */
  def getStatus: String = s"k=$k,kMax=$kMax,t=$currentTemperature,prob=$lastProbability"

  /**
   * The random number generator.
   */
  private val rnd: GenerateRandom = new MersenneTwisterGenerateRandom()
  /**
   * The current global best score.  The global best score is the best score that has been found over all of the
   * iterations.
   */
  private var globalBestScore: Double = Double.PositiveInfinity
  /**
   * The current score.
   */
  private var currentScore: Double = .0
  /**
   * The current iteration.
   */
  private var k: Int = 0
  /**
   * The current temperature.
   */
  private var currentTemperature: Double = .0
  /**
   * The number of cycles to try at each temperature.
   */
  var cycles: Int = 100
  /**
   * The last probability of accepting a new non-improving move.
   */
  private var lastProbability: Double = .0
}
