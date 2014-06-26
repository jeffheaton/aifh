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
package com.heatonresearch.aifh.examples.aco

import com.heatonresearch.aifh.aco.CostGraph
import com.heatonresearch.aifh.aco.DiscreteACO
import com.heatonresearch.aifh.examples.ga.tsp.City

object TSPExampleACO {
  /**
   * Program entry point.
   *
   * @param args Not used.
   */
  def main(args: Array[String]) : Unit = {
    val solve = new TSPExampleACO
    solve.solve()
  }

  /**
   * The number of cities to visit.
   */
  val CITIES = 50
  /**
   * The size of the population.
   */
  val POPULATION_SIZE = 1000
  /**
   * The square size of the map.
   */
  val MAP_SIZE = 256
  /**
   * The maximum number of iterations to allow to have the same score before giving up.
   */
  val MAX_SAME_SOLUTION = 50
}

class TSPExampleACO extends CostGraph {
  import TSPExampleACO._

  /**
   * The cities to visit.
   */
  private var cities: Array[City] = null

  /**
   * Place the cities in random locations.
   */
  private def initCities() {
    cities = Array.ofDim[City](CITIES)
    for(i <- 0 until cities.length) {
      val xPos: Int = (Math.random * MAP_SIZE).asInstanceOf[Int]
      val yPos: Int = (Math.random * MAP_SIZE).asInstanceOf[Int]
      cities(i) = new City(xPos, yPos)
    }
  }

  /**
   * Display the cities in the final path.
   */
  def displaySolution(path: Array[Int]) {
    println(path.mkString(">"))
  }

  /**
   * Setup and solve the TSP.
   */
  def solve() {
    val builder = new StringBuilder
    initCities()
    val aco = new DiscreteACO(this, 50)
    var sameSolutionCount = 0
    var iteration = 1
    var lastSolution = Double.MaxValue
    while (sameSolutionCount < MAX_SAME_SOLUTION) {
      aco.iteration()
      val thisSolution = aco.getBestCost
      builder.setLength(0)
      builder.append("Iteration: ")
      builder.append(iteration)
      iteration += 1
      builder.append(", Best Path Length = ")
      builder.append(thisSolution)
      println(builder.toString())
      if (Math.abs(lastSolution - thisSolution) < 1.0)
        sameSolutionCount += 1
      else
        sameSolutionCount = 0

      lastSolution = thisSolution
    }
    println("Good solution found:")
    val best: Array[Int] = aco.getBestTour
    displaySolution(best)
  }

  def cost(sourceNode: Int, targetNode: Int): Double = {
    val city1: City = cities(sourceNode)
    val city2: City = cities(targetNode)
    city1.proximity(city2)
  }

  def graphSize: Int = CITIES
}