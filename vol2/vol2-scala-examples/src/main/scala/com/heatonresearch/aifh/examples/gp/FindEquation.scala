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
package com.heatonresearch.aifh.examples.gp

import com.heatonresearch.aifh.evolutionary.population.BasicPopulation
import com.heatonresearch.aifh.evolutionary.population.Population
import com.heatonresearch.aifh.evolutionary.species.BasicSpecies
import com.heatonresearch.aifh.evolutionary.train.EvolutionaryAlgorithm
import com.heatonresearch.aifh.evolutionary.train.basic.BasicEA
import com.heatonresearch.aifh.genetic.trees.CrossoverTree
import com.heatonresearch.aifh.genetic.trees.MutateTree
import com.heatonresearch.aifh.genetic.trees.TreeGenome
import com.heatonresearch.aifh.genetic.trees.TreeGenomeFactory
import com.heatonresearch.aifh.learning.score.ScoreRegressionData
import com.heatonresearch.aifh.normalize.DataSet
import com.heatonresearch.aifh.randomize.GenerateRandom
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom
import java.io.FileInputStream
import java.io.IOException
import java.io.InputStream

/**
 * An example that fits an equation to a data file.  This example uses genetic programming.
 */
object FindEquation {
  /**
   * Main entry point.
   *
   * @param args The data file to fit.
   */
  def main(args: Array[String]) {
    val prg = new FindEquation
    if (args.length == 0) {
      prg.process(null)
    }
    else if (args.length == 1) {
      prg.process(args(0))
    }
    else {
      println("Specify a filename to fit, or no filename to use a built in simple polynomial.")
      System.exit(1)
    }
  }

  /**
   * The size of the population.
   */
  val POPULATION_SIZE: Int = 1000
  /**
   * The maximum number of iterations to allow to have the same score before giving up.
   */
  val MAX_SAME_SOLUTION: Int = 500
}

class FindEquation {
  import FindEquation._
  /**
   * Generate a random path through cities.
   */
  private def randomGenome(rnd: GenerateRandom, eval: EvaluateExpression): TreeGenome = {
    val result = new TreeGenome(eval)
    result.root = eval.grow(rnd, 5)
    result
  }

  /**
   * Create an initial random population.
   *
   * @param rnd  A random number generator.
   * @param eval The expression evaluator.
   * @return The new population.
   */
  private def initPopulation(rnd: GenerateRandom, eval: EvaluateExpression): Population = {
    val result: Population = new BasicPopulation(POPULATION_SIZE, null)
    val defaultSpecies = new BasicSpecies(result)
    for(i <- 0 until POPULATION_SIZE) {
      val genome: TreeGenome = randomGenome(rnd, eval)
      defaultSpecies.add(genome)
    }
    result.genomeFactory = new TreeGenomeFactory(eval)
    result.speciesList += defaultSpecies
    result
  }

  /**
   * Process the specified file.
   *
   * @param filename The filename to process.
   */
  def process(filename: String) {
    var iStream: InputStream = null
    if (filename == null) {
      iStream = this.getClass.getResourceAsStream("/simple-poly.csv")
      if (iStream == null) {
        println("Cannot access data set, make sure the resources are available.")
        System.exit(1)
      }
    } else {
      try {
        iStream = new FileInputStream(filename)
      } catch {
        case ex: IOException =>
          ex.printStackTrace()
          System.exit(1)
      }
    }
    val ds = DataSet.load(iStream)
    val training = ds.extractSupervised(0, 1, 1, 1)
    val rnd: GenerateRandom = new MersenneTwisterGenerateRandom
    val eval = new EvaluateExpression(rnd)
    val pop = initPopulation(rnd, eval)
    val score = new ScoreRegressionData(training)
    val genetic: EvolutionaryAlgorithm = new BasicEA(pop, score)
    genetic.addOperation(0.3, new MutateTree(genetic,3))
    genetic.addOperation(0.7, new CrossoverTree(genetic))
    genetic.shouldIgnoreExceptions = false
    var sameSolutionCount = 0
    var iteration = 1
    var lastSolution = Double.MaxValue
    val builder = new StringBuilder
    while (sameSolutionCount < MAX_SAME_SOLUTION) {
      genetic.iteration()
      val thisSolution: Double = genetic.getLastError
      builder.setLength(0)
      builder.append("Iteration: ")
      builder.append(iteration)
      builder.append(", Best Path Length = ")
      builder.append(thisSolution)
      println(builder.toString())
      iteration += 1
      if (Math.abs(lastSolution - thisSolution) < 1.0) {
        sameSolutionCount += 1
      }
      else {
        sameSolutionCount = 0
      }
      lastSolution = thisSolution
    }
    println("Good solution found:")
    val best = genetic.getBestGenome.asInstanceOf[TreeGenome]
    println(eval.displayExpressionNormal(best.root))
    genetic.finishTraining()
  }
}