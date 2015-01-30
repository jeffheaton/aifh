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

import com.heatonresearch.aifh.randomize.GenerateRandom
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom

/**
 * The ant colony optimization (ACO) algorithm finds an optimal path through a graph.  It works by establishing
 * pheromone trails between the graph nodes.  The pheromone trails increase in strength as ants travel over the
 * edges of the graph.  The pheromone trails decrease over time.  The discrete version of ACO arranges a path
 * to visit the nodes of a graph, that minimizes cost.
 * <p/>
 * References:
 * <p/>
 * http://en.wikipedia.org/wiki/Ant_colony_optimization_algorithms
 * <p/>
 * M. Dorigo, Optimization, Learning and Natural Algorithms, PhD thesis, Politecnico di Milano, Italy, 1992.
 */
object DiscreteACO {
  /**
   * The initial value of the pheromone trails.
   */
  var INITIAL_PHEROMONE = 1.0
}

/**
 * @param graph    The graph that we are seeking a minimal path through.
 * @param antCount The number of ants to use.
 */
class DiscreteACO(val graph: CostGraph, antCount: Int) {

  val len: Int = graph.graphSize

  /** The pheromone trails between graph segments. */
  val pheromone = Array.ofDim[Double](len, len)

  /** The current best path. */
  val bestPath = new Array[Int](len)

  /** The cost of the best path.  We are trying to minimize  */
  private var bestCost = Double.PositiveInfinity

  for(i <- 0 until len;
      j <- 0 until len) {
    pheromone(i)(j) = DiscreteACO.INITIAL_PHEROMONE
  }

  /**
   * The ants.
   */
  val ants: List[DiscreteAnt] = List.fill(antCount)(new DiscreteAnt(len))

  /**
   * Constant that defines the attractiveness of the pheromone trail.
   */
  var alpha: Double = 1
  /**
   * Constant that defines the attractiveness of better state transitions (from one node to another).
   */
  var beta: Double = 5
  /**
   * Constant that defines how quickly the pheromone path evaporates.
   */
  var evaporation: Double = 0.5
  /**
   * The amount of pheromone that the nodes of a path share for a trip.
   */
  var q: Double = 500
  /**
   * The base probability.
   */
  var pr: Double = 0.01
  /**
   * A random number generator.
   */
  var random: GenerateRandom = new MersenneTwisterGenerateRandom

  /**
   * Calculate the probability of a given ant moving to any of the next nodes.
   *
   * @param currentIndex The index into the path.
   * @param ant          The ant.
   * @return The probability of moving to the next node.
   */
  private def calculateProbability(currentIndex: Int, ant: DiscreteAnt): Array[Double] = {
    val result = new Array[Double](graph.graphSize)
    val i: Int = ant.path(currentIndex - 1)
    var d = 0.0
    for(l <- 0 until graph.graphSize) {
      if (!ant.wasVisited(l))
        d += Math.pow(pheromone(i)(l), alpha) * Math.pow(1.0 / graph.cost(i, l), beta)
    }
    for(j <- 0 until graph.graphSize) {
      if (ant.wasVisited(j)) {
        result(j) = 0.0
      }
      else {
        val n: Double = Math.pow(pheromone(i)(j), alpha) * Math.pow(1.0 / graph.cost(i, j), beta)
        result(j) = n / d
      }
    }
    result
  }

  /**
   * Choose the next node for an ant to visit.  This is based on probability.
   *
   * @param currentIndex The step we are at in the path.
   * @param ant          The ant being evaluated.
   * @return The node we will move into.
   */
  private def pickNextNode(currentIndex: Int, ant: DiscreteAnt): Int = {
    if (currentIndex == 0 || random.nextDouble < pr) {
      var index: Int = 0
      do {
        index = random.nextInt(0, graph.graphSize)
      } while (ant.wasVisited(index))
      return index
    }
    val prob: Array[Double] = calculateProbability(currentIndex, ant)
    val r = random.nextDouble()
    var sum: Double = 0
    for(i <- 0 until graph.graphSize) {
      sum += prob(i)
      if (sum >= r) return i
    }
    -1
  }

  /**
   * Update the pheromone levels both for ants traveling and evaporation.
   */
  private def updatePheromone() {
    for(i <- 0 until pheromone.length;
        j <- 0 until pheromone(i).length) {
      pheromone(i)(j) *= evaporation
    }
    for (a <- ants) {
      val d = q / a.calculateCost(graph.graphSize, graph)
      for(i <- 0 until (graph.graphSize - 1)) {
        pheromone(a.path(i))(a.path(i + 1)) += d
      }
      pheromone(a.path(graph.graphSize - 1))(a.path(0)) += d
    }
  }

  /**
   * Move the ants forward on their path.
   */
  private def march() {
    for(currentIndex <- 0 until graph.graphSize) {
      for (a <- ants) {
        val next: Int = pickNextNode(currentIndex, a)
        a.visit(currentIndex, next)
      }
    }
  }

  /**
   * Reset the ants.
   */
  private def setupAnts() {
    for (a <- ants) {
      a.clear()
    }
  }

  /**
   * Update the best path.
   */
  private def updateBest() {
    var bestPathFound: Array[Int] = null
    for (a <- ants) {
      val cost = a.calculateCost(graph.graphSize, graph)
      if (cost < bestCost) {
        bestPathFound = a.path
        bestCost = cost
      }
    }
    if (bestPathFound != null) {
      System.arraycopy(bestPathFound, 0, bestPath, 0, bestPath.length)
    }
  }

  /**
   * Perform one iteration.
   */
  def iteration() {
    setupAnts()
    march()
    updatePheromone()
    updateBest()
  }

  /**
   * @return The best tour/path.
   */
  def getBestTour: Array[Int] = bestPath

  def getBestCost: Double = bestCost
}