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
package com.heatonresearch.aifh.examples.ca.mergelife.universe

import com.heatonresearch.aifh.examples.ca.mergelife.physics.Physics
import com.heatonresearch.aifh.randomize.GenerateRandom
import java.util.concurrent.locks.Lock
import java.util.concurrent.locks.ReentrantLock

/**
 * Used to run a universe's physics and provide display.
 *
 * @param universe The universe.
 * @param physics  The physics calculator.
 */
class UniverseRunner(val universe: Universe,val physics: Physics) {

  /**
   * The event used to sync waiting for tasks to stop.
   */
  private val accessLock: Lock = new ReentrantLock
  /**
   * Should we automatically kill a universe that stabilizes.
   */
  var autoKill: Boolean = false
  /**
   * The difference between two frames.
   */
  private var diff: Double = .0
  /**
   * The current iteration.
   */
  private var iteration: Int = 0
  /**
   * The universe that is used to generate the next frame.
   */
  private val tempUniverse = universe.clone.asInstanceOf[Universe]

  /**
   * Advance one frame.
   */
  def advance(rnd: GenerateRandom) {
    val height = this.universe.getHeight
    val width = this.universe.getWidth
    try {
      this.accessLock.lock()
      this.tempUniverse.copy(this.universe)
      for(col <- 0 until width;
          row <- 0 until height) {
        this.physics.processPixel(this.tempUniverse, row, col)
      }
      this.diff = this.tempUniverse.compare(this.universe)
      this.iteration += 1
      this.universe.copy(this.tempUniverse)
      if (this.diff < 0.0001 && this.iteration > 5) {
        if (this.autoKill) {
          reset(rnd)
        }
      }
    }
    finally {
      this.accessLock.unlock()
    }
  }

  /**
   * Perform a genetic crossover between two parent universes.  A new universe will be created with attributes
   * from the two parents.
   *
   * @param rnd              Random number generator.
   * @param crossoverParent1 The first parent.
   * @param crossoverParent2 The second parent.
   */
  def crossover(rnd: GenerateRandom, crossoverParent1: UniverseRunner, crossoverParent2: UniverseRunner) {
    val parent1: Array[Double] = crossoverParent1.physics.getData
    val parent2: Array[Double] = crossoverParent2.physics.getData
    val child: Array[Double] = physics.getData
    val len: Int = parent1.length
    val p1: Int = (rnd.nextDouble * len.asInstanceOf[Double]).asInstanceOf[Int]
    val p2: Int = (rnd.nextDouble * len.asInstanceOf[Double]).asInstanceOf[Int]
    for(i <- 0 until physics.getData.length) {
      if (i < p1) {
        child(i) = parent1(i)
      }
      else if (i >= p1 && i <= p2) {
        child(i) = parent2(i)
      }
      else if (i > p2) {
        child(i) = parent1(i)
      }
    }
  }

  /**
   * @return The difference between the last two frames.
   */
  def getDiff: Double = diff

  /**
   * @return The total number of iterations.
   */
  def getIterations: Int = iteration

  /**
   * @return the autoKill
   */
  def isAutoKill: Boolean = autoKill

  /**
   * Perform a mutate on a parent and generate a new child.  The parent is not changed.
   *
   * @param rnd           Random number generator.
   * @param sourcePhysics The parent object.
   * @param probChange    The probability of changing an individual element.
   * @param perturb       The amount that an object is changed by.
   */
  def mutate(rnd: GenerateRandom, sourcePhysics: Physics, probChange: Double, perturb: Double) {
    physics.copyData(sourcePhysics.getData)
    for(i <- 0 until sourcePhysics.getData.length) {
      if (rnd.nextDouble < probChange) {
        physics.getData(i) += perturb * rnd.nextDouble(-1, 1)
      }
    }
  }

  /**
   * Randomize the universe grid.
   *
   * @param rnd Random number generator.
   */
  def randomize(rnd: GenerateRandom) {
    this.accessLock.lock()
    this.universe.randomize(rnd)
    this.iteration = 0
    this.accessLock.unlock()
  }

  /**
   * Randomize the universe grid and the physics calculator.
   *
   * @param rnd A random number generator.
   */
  def reset(rnd: GenerateRandom) {
    this.accessLock.lock()
    this.physics.randomize()
    this.universe.randomize(rnd)
    this.iteration = 0
    this.accessLock.unlock()
  }

  override def toString: String = "Iteration: " + this.iteration + ", Diff=" + this.diff
}