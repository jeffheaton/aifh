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
import com.heatonresearch.aifh.general.data.RichData._

/**
 * Train a Machine Learning Algorithm using Simulated Annealing.  Simulated Annealing is a Monte Carlo algorithm that
 * is based on annealing in metallurgy, a technique involving heating and controlled cooling of a material to increase
 * the size of its crystals and reduce their defects, both are attributes of the material that depend on its
 * thermodynamic free energy.
 * <p/>
 * The Simulated Annealing algorithm works by randomly changing a vector of doubles.  This is the long term memory of
 * the Machine Learning algorithm.  While this happens a temperature is slowly decreased.  When this temperature is
 * higher, the Simulated Annealing algorithm is more likely to accept changes that have a higher error (or energy)
 * than the current state.
 * <p/>
 * There are several important components to any Simulated Learning Algorithm:
 * <p/>
 * First, the randomization technique.  This is performed by the method performRandomize.  To randomize differently,
 * override this method.
 * <p/>
 * Secondly, the cooling schedule.  This determines how quickly the current temperature will fall.  This is controlled
 * by the coolingSchedule.  To define a different cooling schedule, override this method.
 * <p/>
 * Finally, the probability of accepting a higher-error (energy) solution.  This is defined by a Probability
 * Distribution Function (PDF) contained in calcProbability.  To define a different PDF, override this method.
 * <p/>
 * http://en.wikipedia.org/wiki/Simulated_annealing
 * @param algorithm           The machine learning algorithm to optimize.
 * @param score               The scoring function, this determines the energy (error) of the current solution.
 * @param kMax                The max number of iterations.
 * @param startingTemperature The starting temperature.
 * @param endingTemperature   Do not set to zero, as many cooling schedules asymptotically approach zero.
 * Rather, use something close to zero, like 0.0001.
 */
class TrainAnneal(algorithm: MachineLearningAlgorithm, val score: ScoreFunction,
                  val kMax: Int, val startingTemperature: Double, val endingTemperature: Double) extends LearningMethod {

  /**
   * Construct the simulated annealing trainer.  Use 1000 iterations and temperature from 400 to 0.0001.
   */
  def this(theAlgorithm: MachineLearningAlgorithm, theScore: ScoreFunction) {
    this(theAlgorithm, theScore, 1000, 400, 0.0001)
  }

  /**
   * The current error.
   */
  private var currentError: Double = score.calculateScore(algorithm)

  /**
   * The current best solution ever found.
   */
  private var globalBest: ArrayBuffer[Double] = algorithm.longTermMemory.clone()

  /**
   * The cooling schedule.  This is a Probability Distribution Function (PDF) that specifies the probability,
   * at a given temperature, of accepting a higher-energy move.
   *
   * @return The probability.
   */
  def coolingSchedule: Double = {
    val ex = k.toDouble / kMax.toDouble
    startingTemperature * Math.pow(endingTemperature / startingTemperature, ex)
  }

  override def iteration() {
    k += 1
    currentTemperature = coolingSchedule
    for(cycle <- 0 until cycles) {
      // backup current state
      val oldState = algorithm.longTermMemory.clone()

      // randomize the method
      performRandomize(algorithm.longTermMemory)

      // did we improve it? Only keep the new method if it improved (greedy).
      val trialError = score.calculateScore(algorithm)

      // keep or reset to previous state
      if ((trialError < currentError) || calcProbability(currentError, trialError, currentTemperature) > rnd.nextDouble) {
        currentError = trialError
        
        // better than global error
        if (trialError < globalBestError) {
          globalBestError = trialError
          globalBest = algorithm.longTermMemory.clone()
        }
      }
      else {
        algorithm.longTermMemory.set(oldState)
      }
    }
  }

  /**
   * Randomly move to a new location.  To specify a new randomization function, override this method.
   *
   * @param memory The long term memory.
   */
  def performRandomize(memory: ArrayBuffer[Double]) {
    for(i <- 0 until memory.length)
      memory(i) += rnd.nextGaussian * 3
  }

  /**
   * @return True, if we have reached the max iterations.
   */
  def done: Boolean = k >= kMax

  /**
   * @return The error (or energy) from the last iteration.
   */
  def getLastError: Double = globalBestError

  /**
   * Calculate the probability that we will accept a move that takes us to a higher energy (higher error)
   * position.
   *
   * @param ecurrent The current energy.
   * @param enew     The new energy if we move.
   * @param t        The current temperature.
   * @return The probability.
   */
  def calcProbability(ecurrent: Double, enew: Double, t: Double): Double = Math.exp(-(Math.abs(enew - ecurrent) / t))

  /**
   * @return The current temperature.
   */
  def getCurrentTemperature: Double = currentTemperature

  /**
   * @return The current iteration number.
   */
  def getK: Int = k

  /**
   * @return The last probability.
   */
  def getLastProbability: Double = lastProbability

  /**
   * Copy the global best solution to the machine learning algorithm.  It is very important to call this method.
   */
  def finishTraining() {
    algorithm.longTermMemory.clear()
    algorithm.longTermMemory ++= globalBest
  }

  override def getStatus: String = s"k=$k,kMax=$kMax,t=$currentTemperature,prob=$lastProbability"

  /**
   * The random number generator to use.
   */
  private val rnd: GenerateRandom = new MersenneTwisterGenerateRandom()
  /**
   * The current error of best solution ever found.
   */
  private var globalBestError: Double = Double.PositiveInfinity

  /**
   * The current iteration number.
   */
  private var k: Int = 0
  /**
   * The current temperature.
   */
  private var currentTemperature: Double = .0

  /**
   * The number of random moves to try for each iteration.
   */
  var cycles: Int = 100

  /**
   * The probability for the last iteration cycle.
   */
  private var lastProbability: Double = .0
}
