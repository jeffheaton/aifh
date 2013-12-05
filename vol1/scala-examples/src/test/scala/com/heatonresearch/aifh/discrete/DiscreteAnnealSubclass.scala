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

import com.heatonresearch.aifh.distance.{CalculateDistance, EuclideanDistance}
import com.heatonresearch.aifh.randomize.GenerateRandom
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom
import com.heatonresearch.aifh.general.data.RichData
import RichData._

/**
 * A simple test subclass for discrete anneal.
 */
object DiscreteAnnealSubclass {
  val IDEAL = Vector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0)
}

class DiscreteAnnealSubclass(theKMax: Int, theStartingTemperature: Double, theEndingTemperature: Double)
  extends DiscreteAnneal(theKMax, theStartingTemperature, theEndingTemperature) {
  cycles = 1000

  def backupState() {
    backup = current
  }

  def restoreState() {
    current = backup
  }

  def foundNewBest() {
    best = current
  }

  def moveToNeighbor() {
    val pt1 = rnd.nextInt(current.size)
    var pt2 = 0
    do {
      pt2 = rnd.nextInt(current.size)
    } while (pt1 == pt2)

    current = current.swap(pt1,pt2)
  }

  def getBest: Vector[Double] = best

  def evaluate: Double = distance.calculate(DiscreteAnnealSubclass.IDEAL, current)

  private var current = Vector(10.0, 9.0, 8.0, 7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0)
  private var backup = Vector[Double]()
  private var best = Vector[Double]()
  private val rnd: GenerateRandom = new MersenneTwisterGenerateRandom(1)
  private val distance: CalculateDistance = new EuclideanDistance
}