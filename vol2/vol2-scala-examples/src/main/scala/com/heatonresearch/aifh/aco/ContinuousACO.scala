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
package com.heatonresearch.aifh.aco

import com.heatonresearch.aifh.AIFH
import com.heatonresearch.aifh.learning.LearningMethod
import com.heatonresearch.aifh.learning.MLMethod
import com.heatonresearch.aifh.learning.score.ScoreFunction
import com.heatonresearch.aifh.randomize.GenerateRandom
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom

/**
* This class implements continuous ant colony optimization (CACO)
* <p/>
* References:
* <p/>
* Training Neural Networks with Ant Colony Optimization,
* Arun Pandian, Spring, 2013
* <p/>
* Krzysztof Socha and Christian Blum. “An ant colony optimization algorithm for
* continuous optimization: application to feed-forward neural network training”, in
* Springer London (2007).
* <p/>
* M.Dorigo, V.Maniezzo, and A.Colorni. “Ant System: Optimization by a colony of
* cooperating agents”, in IEEE Transactions on Systems, Man, and Cybernetics,
* 1996.
*/
object ContinuousACO {
  /**
   * Sigma constant. Minimum standard deviation.
   */
  val CONST_SIGMA = 0.1
  /**
   * Q constant.  Weighting exponent factor.
   */
  val CONST_Q = 0.08
}

/**
 *
 * @param algorithm The algorithm to fit.
 * @param score The score function
 * @param populationSize The population size.
 */
class ContinuousACO(val algorithm : MLMethod,score: ScoreFunction,val populationSize : Int) extends LearningMethod {

  import ContinuousACO._

  /** Random number generation. */
  var random: GenerateRandom = new MersenneTwisterGenerateRandom
  /** The parameter count. */
  private val paramCount: Int = algorithm.getLongTermMemory.length

  /** The population of ants. */
  private val population = {
    val pop = new Array[ContinuousAnt](populationSize * 2)
    for(i <- 0 until pop.length) {
      pop(i) = new ContinuousAnt(paramCount, score.shouldMinimize)
      for(j <- 0 until paramCount)
        pop(i).params(j) = random.nextDouble(-1, 1)
    }
    pop
  }

  /** The weighting of each ant. */
  private val weighting = new Array[Double](populationSize)

  /**
   * The sum of the weighting.
   */
  private var sumWeighting: Double = 0
  /**
   * Epsilon, learning rate.
   */
  var epsilon: Double = .75


  updateScore()
  java.util.Arrays.sort(population.asInstanceOf[Array[Object]])
  computeWeighting()
  sampleSolutions()
  java.util.Arrays.sort(population.asInstanceOf[Array[Object]])

  /**
   * Update the score.
   */
  private def updateScore() {
    for (aPopulation <- population) {
      System.arraycopy(aPopulation.params, 0, algorithm.getLongTermMemory, 0, paramCount)
      aPopulation.score = score.calculateScore(algorithm)
    }
  }

  /**
   * Compute the weighting for each ant.
   */
  private def computeWeighting() {
    sumWeighting = 0
    val coef = (1 / (0.1 * Math.sqrt(2 * Math.PI)))
    for(i <- 0 until populationSize) {
      val exponent = (i * i) / (2 * CONST_Q * CONST_Q * populationSize * populationSize)
      weighting(i) = coef * Math.exp(-exponent)
      sumWeighting += weighting(i)
    }
  }

  /**
   * Compute the standard deviation.
   *
   * @param x The parameter to compute for.
   * @param l The population member.
   * @return The standard deviation.
   */
  private def computeSD(x: Int, l: Int): Double = {
    var sum: Double = 0.0
    for(i <- 0 until populationSize) {
      sum += Math.abs(population(i).params(x) - population(l).params(x)) / (populationSize - 1)
    }

    if (sum < AIFH.DEFAULT_PRECISION) {
      return CONST_SIGMA
    }
    epsilon * sum
  }


  /**
   * Select a probability distribution function (PDF).
   *
   * @return The PDF index.
   */
  private def selectPDF: Int = {
    class BreakException extends Exception

    var l = 0
    var temp = 0.0
    val r = random.nextDouble()
    try {
      for(i <- 0 until populationSize) {
        temp += weighting(i) / sumWeighting
        if (r < temp) {
          l = i
          throw new BreakException
        }
      }
    } catch {
      case b : BreakException =>
    }
    l
  }

  /**
   * Sample new parameters.
   */
  private def sampleSolutions() {
    for(i <- populationSize until population.length;
        pdf = selectPDF ;
        j <- 0 until paramCount) {
        val sigma: Double = computeSD(j, pdf)
        val mu: Double = population(pdf).params(j)
        val d: Double = (random.nextGaussian * sigma) + mu
        population(i).params(j) = d
    }
  }

  override def iteration() {
    computeWeighting()
    sampleSolutions()
    updateScore()
    java.util.Arrays.sort(population.asInstanceOf[Array[Object]])
  }

  override def getLastError: Double = population(0).score

  override def done: Boolean = false

  override def getStatus: String = ""

  override def finishTraining() {
    System.arraycopy(population(0).params, 0, algorithm.getLongTermMemory, 0, algorithm.getLongTermMemory.length)
  }
}