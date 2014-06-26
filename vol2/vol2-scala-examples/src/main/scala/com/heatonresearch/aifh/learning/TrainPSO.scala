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
package com.heatonresearch.aifh.learning

import com.heatonresearch.aifh.general.VectorAlgebra
import com.heatonresearch.aifh.learning.score.ScoreFunction
import com.heatonresearch.aifh.randomize.GenerateRandom
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom

object TrainPSO {
  val maxVelocity: Double = 2

  def apply(particles: Array[MLMethod], score: ScoreFunction) : TrainPSO = {
    val rnd: GenerateRandom = new MersenneTwisterGenerateRandom

    val vectorSize: Int = particles(0).getLongTermMemory.length
    val particleCount: Int = particles.length
    val bestVectors = Array.ofDim[Double](particleCount, vectorSize)
    val velocities = Array.ofDim[Double](particleCount, vectorSize)
    val bestScores = Array.ofDim[Double](particleCount)
    for (velocity <- velocities) {
      VectorAlgebra.randomise(rnd, velocity, this.maxVelocity)
    }
    new TrainPSO(particles,rnd,score,bestVectors,velocities,bestScores,vectorSize : Int)
  }



}

/**
 * Iteratively trains a population by applying
 * particle swarm optimisation (PSO).
 * <p/>
 * Based on Encog contribution by:
 * Geoffroy Noel, https://github.com/goffer-looney
 * <p/>
 * References:
 * James Kennedy and Russell C. Eberhart, Particle swarm optimization,
 * Proceedings of the IEEE International Conference on Neural Networks,
 * 1995, pp. 1942-1948
 *
 * @author Geoffroy Noel
 */
class TrainPSO private (val particles: Array[MLMethod],
                        val rnd : GenerateRandom,
                        val score: ScoreFunction,
                        val bestVectors: Array[Array[Double]],
                        val velocities: Array[Array[Double]],
                        val bestScores: Array[Double],
                        vectorSize : Int
                         ) extends LearningMethod {
  /**
   * Swarm state and memories.
   */
  protected var bestVectorIndex: Int = -1
  /**
   * Although this is redundant with m_bestVectors[bestVectorIndex],
   * bestVectors[bestVectorIndex] is not thread safe.
   */
  private val bestVector: Array[Double] = new Array[Double](vectorSize)

  protected var maxPosition: Double = -1
  protected var c1: Double = 2.0
  protected var c2: Double = 2.0
  private var bestScore: Double = .0
  protected var inertiaWeight: Double = 0.4

  import TrainPSO._
  /**
   * Update the velocity, position and personal
   * best position of a particle
   *
   * @param particleIndex index of the particle in the swarm
   */
  protected def updateParticle(particleIndex: Int) {
    val particlePosition: Array[Double] = this.particles(particleIndex).getLongTermMemory
    updateVelocity(particleIndex)
    VectorAlgebra.clampComponents(velocities(particleIndex), maxVelocity)
    VectorAlgebra.add(particlePosition, velocities(particleIndex))
    VectorAlgebra.clampComponents(particlePosition, maxPosition)
    updatePersonalBestPosition(particleIndex, particlePosition)
  }

  /**
   * Update the velocity of a particle
   *
   * @param particleIndex index of the particle in the swarm
   */
  protected def updateVelocity(particleIndex: Int) {
    val particlePosition: Array[Double] = this.particles(particleIndex).getLongTermMemory
    val vtmp: Array[Double] = new Array[Double](particlePosition.length)
    VectorAlgebra.mul(velocities(particleIndex), inertiaWeight)
    VectorAlgebra.copy(vtmp, bestVectors(particleIndex))
    VectorAlgebra.sub(vtmp, particlePosition)
    VectorAlgebra.mulRand(this.rnd, vtmp, this.c1)
    VectorAlgebra.add(velocities(particleIndex), vtmp)
    if (particleIndex != bestVectorIndex) {
      VectorAlgebra.copy(vtmp, bestVector)
      VectorAlgebra.sub(vtmp, particlePosition)
      VectorAlgebra.mulRand(this.rnd, vtmp, c2)
      VectorAlgebra.add(velocities(particleIndex), vtmp)
    }
  }

  /**
   * Update the personal best position of a particle.
   *
   * @param particleIndex    index of the particle in the swarm
   * @param particlePosition the particle current position vector
   */
  protected def updatePersonalBestPosition(particleIndex: Int, particlePosition: Array[Double]) {
    val score: Double = this.score.calculateScore(this.particles(particleIndex))
    if ((bestScores(particleIndex) == 0) || isScoreBetter(score, bestScores(particleIndex))) {
      bestScores(particleIndex) = score
      VectorAlgebra.copy(bestVectors(particleIndex), particlePosition)
    }
  }

  /**
   * Update the swarm's best position
   */
  protected def updateGlobalBestPosition() {
    var bestUpdated: Boolean = false
    for(i <- 0 until particles.length) {
      if ((bestVectorIndex == -1) || isScoreBetter(bestScores(i), bestScores(bestVectorIndex))) {
        bestVectorIndex = i
        bestUpdated = true
      }
    }
    if (bestUpdated) {
      VectorAlgebra.copy(bestVector, bestVectors(bestVectorIndex))
      this.bestScore = bestScores(bestVectorIndex)
    }
  }

  /**
   * Compares two scores.
   *
   * @param score1 a score
   * @param score2 a score
   * @return true if score1 is better than score2
   */
  private[learning] def isScoreBetter(score1: Double, score2: Double): Boolean = {
    (this.score.shouldMinimize && (score1 < score2)) || ((!this.score.shouldMinimize) && (score1 > score2))
  }

  override def iteration() {
    for(i <- 0 until particles.length)
      updateParticle(i)
    updateGlobalBestPosition()
  }

  /**
   * @return False, this algorithm can be iterated an unlimited number of times.
   */
  def done: Boolean = false

  /**
   * @return The error (or energy) from the last iteration.
   */
  def getLastError: Double = bestScore

  /**
   * Calculate the probability that we will accept a move that takes us to a higher energy (higher error)
   * position.
   *
   * @param eCurrent The current energy.
   * @param eNew     The new energy if we move.
   * @param t        The current temperature.
   * @return The probability.
   */
  def calcProbability(eCurrent: Double, eNew: Double, t: Double): Double = Math.exp(-(Math.abs(eNew - eCurrent) / t))

  /**
   * Copy the global best solution to the machine learning algorithm.  It is very important to call this method.
   */
  override def finishTraining() {}

  override def getStatus: String = ""

  def getBestParticle: MLMethod = {
    VectorAlgebra.copy(this.particles(bestVectorIndex).getLongTermMemory, this.bestVector)
    particles(bestVectorIndex)
  }
}