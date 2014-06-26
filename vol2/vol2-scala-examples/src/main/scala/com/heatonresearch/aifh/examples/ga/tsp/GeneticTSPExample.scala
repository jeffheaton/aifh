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
package com.heatonresearch.aifh.examples.ga.tsp

import com.heatonresearch.aifh.evolutionary.population.BasicPopulation
import com.heatonresearch.aifh.evolutionary.population.Population
import com.heatonresearch.aifh.evolutionary.species.BasicSpecies
import com.heatonresearch.aifh.evolutionary.train.basic.BasicEA
import com.heatonresearch.aifh.genetic.crossover.SpliceNoRepeat
import com.heatonresearch.aifh.genetic.genome.IntegerArrayGenome
import com.heatonresearch.aifh.genetic.genome.IntegerArrayGenomeFactory
import com.heatonresearch.aifh.genetic.mutate.MutateShuffle
import com.heatonresearch.aifh.learning.score.ScoreFunction

/**
 * Find the shortest path through several cities with a genetic algorithm (GA).
 * This example shows how to use it to find a potential solution to the Traveling Salesman Problem (TSP).
 */
object GeneticTSPExample {
  /**
   * Program entry point.
   *
   * @param args Not used.
   */
  def main(args: Array[String]) {
    val solve = new GeneticTSPExample
    solve.solve()
  }

  /**
   * The number of cities to visit.
   */
  val CITIES: Int = 50
  /**
   * The size of the population.
   */
  val POPULATION_SIZE: Int = 1000
  /**
   * The square size of the map.
   */
  val MAP_SIZE: Int = 256
  /**
   * The maximum number of iterations to allow to have the same score before giving up.
   */
  val MAX_SAME_SOLUTION: Int = 50
}

class GeneticTSPExample {
  import GeneticTSPExample._

  /**
   * The genetic algorithm.
   */
  private var genetic: BasicEA = null
  /**
   * The cities to visit.
   */
  private var cities: Array[City] = null

  /**
   * Place the cities in random locations.
   */
  private def initCities() {
    cities = Array.fill[City](CITIES){
      val xPos: Int = (Math.random * MAP_SIZE).asInstanceOf[Int]
      val yPos: Int = (Math.random * MAP_SIZE).asInstanceOf[Int]
      new City(xPos, yPos)
    }
  }

  /**
   * Generate a random path through cities.
   */
  private def randomGenome: IntegerArrayGenome = {
    val result: IntegerArrayGenome = new IntegerArrayGenome(cities.length)
    val organism: Array[Int] = result.getData
    val taken: Array[Boolean] = new Array[Boolean](cities.length)
    for(i <- 0 until (organism.length -1)) {
      var icandidate: Int = 0
      do {
        icandidate = (Math.random * organism.length).asInstanceOf[Int]
      } while (taken(icandidate))
      organism(i) = icandidate
      taken(icandidate) = true
      if (i == organism.length - 2) {
        icandidate = 0
        while (taken(icandidate)) {
          icandidate += 1
        }
        organism(i + 1) = icandidate
      }
    }
    result
  }

  /**
   * Create an initial random population of random paths through the cities.
   *
   * @return The random population.
   */
  private def initPopulation: Population = {
    val result = new BasicPopulation(POPULATION_SIZE, null)
    val defaultSpecies = new BasicSpecies(result)
    for(i <- 0 until POPULATION_SIZE) {
      val genome = randomGenome
      defaultSpecies.add(genome)
    }
    result.genomeFactory = new IntegerArrayGenomeFactory(cities.length)
    result.speciesList += defaultSpecies
    result
  }

  /**
   * Display the cities in the final path.
   */
  def displaySolution(solution: IntegerArrayGenome) {
    val path = solution.getData
    println(path.mkString(">"))
  }

  /**
   * Setup and solve the TSP.
   */
  def solve() {
    val builder = new StringBuilder
    initCities()
    val pop = initPopulation
    val score: ScoreFunction = new TSPScore(cities)
    genetic = new BasicEA(pop, score)
    genetic.addOperation(0.9, new SpliceNoRepeat(genetic,CITIES / 3))
    genetic.addOperation(0.1, new MutateShuffle(genetic))
    var sameSolutionCount: Int = 0
    var iteration = 1
    var lastSolution = Double.MaxValue
    while (sameSolutionCount < MAX_SAME_SOLUTION) {
      genetic.iteration()
      val thisSolution = genetic.getLastError
      builder.setLength(0)
      builder.append("Iteration: ")
      builder.append(iteration)
      iteration += 1
      builder.append(", Best Path Length = ")
      builder.append(thisSolution)
      println(builder.toString())
      if (Math.abs(lastSolution - thisSolution) < 1.0) {
        sameSolutionCount += 1
      }
      else {
        sameSolutionCount = 0
      }
      lastSolution = thisSolution
    }
    println("Good solution found:")
    val best = genetic.getBestGenome.asInstanceOf[IntegerArrayGenome]
    displaySolution(best)
    genetic.finishTraining()
  }
}