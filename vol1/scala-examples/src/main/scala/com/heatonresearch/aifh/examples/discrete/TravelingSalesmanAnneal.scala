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
package com.heatonresearch.aifh.examples.discrete

import com.heatonresearch.aifh.discrete.DiscreteAnneal
import com.heatonresearch.aifh.distance.{CalculateDistance, EuclideanDistance}
import com.heatonresearch.aifh.randomize.GenerateRandom
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom

/**
 * Use simulated annealing with the Traveling Salesman Problem (TSP).  The cities are placed in a circle, so the
 * ideal path is known.  Because the cities are in a circle they should be visited in order for the absolute
 * optimal path.
 * <p/>
 * http://en.wikipedia.org/wiki/Traveling_salesman_problem
 */
object TravelingSalesmanAnneal extends App {
  /**
   * The size of the map.
   */
  val MAP_SIZE: Double = 10
  /**
   * The city count.
   */
  val CITY_COUNT: Int = 50

  (new TravelingSalesmanAnneal).run()
}

class TravelingSalesmanAnneal extends DiscreteAnneal(1000, 400, 0.001) {

  import TravelingSalesmanAnneal._

  /**
   * The city coordinates.
   */
  private val cities: IndexedSeq[Vector[Double]] = {
    val ratio = (2 * Math.PI) / CITY_COUNT
    for(cityNumber <- 0 until CITY_COUNT) yield {
      val v1 = (Math.cos(ratio * cityNumber) * (MAP_SIZE / 2) + (MAP_SIZE / 2)).toInt
      val v2 = (Math.sin(ratio * cityNumber) * (MAP_SIZE / 2) + (MAP_SIZE / 2)).toInt
      Vector[Double](v1,v2)
    }
  }

  private def swap[V](vec: Vector[V], pos1: Int, pos2: Int): Vector[V] = {
    val temp = vec(pos1)
    vec.updated(pos1,vec(pos2)).updated(pos2,temp)
  }
  /**
   * Run the example.
   */
  def run() {
    backupPath = Vector[Int](CITY_COUNT)
    bestPath = Vector[Int](CITY_COUNT)

    currentPath = (0 until CITY_COUNT).toVector
    for(i <- 0 until CITY_COUNT) {
      val pos2 = rnd.nextInt(CITY_COUNT)
      currentPath = swap(currentPath,i,pos2)
    }

    while (!done) {
      iteration()
      println("Iteration #" + getK + ", Best Score=" + getBestScore + "," + getStatus)
    }
    println(bestPath.mkString("[",", ","]"))
  }

  override def backupState() {
    backupPath = currentPath
  }

  override def restoreState() {
    currentPath = backupPath
  }

  override def foundNewBest() {
    bestPath = currentPath
  }

  override def moveToNeighbor() {
    val pt1: Int = rnd.nextInt(currentPath.length)
    var pt2: Int = 0
    do {
      pt2 = rnd.nextInt(currentPath.length)
    } while (pt1 == pt2)

    currentPath = swap(currentPath,pt1,pt2)
  }

  override def evaluate: Double = {
    cities.zip(cities.tail).map { p => distance.calculate(p._1,p._2) }.sum
  }

  /**
   * The distance calculator.
   */
  private val distance: CalculateDistance = new EuclideanDistance
  /**
   * A random number generator.
   */
  private val rnd: GenerateRandom = new MersenneTwisterGenerateRandom()
  /**
   * The current path being evaluated.
   */
  private var currentPath: Vector[Int] = Vector.empty[Int]
  /**
   * The backup path, in case the current is not kept.
   */
  private var backupPath: Vector[Int] = Vector.empty[Int]
  /**
   * The best path yet.
   */
  private var bestPath: Vector[Int] = Vector.empty[Int]
}
