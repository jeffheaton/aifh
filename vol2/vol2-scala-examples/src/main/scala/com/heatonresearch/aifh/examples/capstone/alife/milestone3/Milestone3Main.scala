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
package com.heatonresearch.aifh.examples.capstone.alife.milestone3

import com.heatonresearch.aifh.evolutionary.population.BasicPopulation
import com.heatonresearch.aifh.evolutionary.population.Population
import com.heatonresearch.aifh.evolutionary.species.BasicSpecies
import com.heatonresearch.aifh.evolutionary.train.basic.BasicEA
import com.heatonresearch.aifh.examples.capstone.alife.milestone1.DisplayPlant
import com.heatonresearch.aifh.examples.capstone.alife.milestone1.PlantUniverse
import com.heatonresearch.aifh.examples.capstone.alife.milestone2.PlantGrowth
import com.heatonresearch.aifh.examples.capstone.alife.milestone2.PlantPhysics
import com.heatonresearch.aifh.genetic.crossover.Splice
import com.heatonresearch.aifh.genetic.genome.DoubleArrayGenome
import com.heatonresearch.aifh.genetic.genome.DoubleArrayGenomeFactory
import com.heatonresearch.aifh.genetic.mutate.MutatePerturb
import com.heatonresearch.aifh.genetic.species.ArraySpeciation
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom
import javax.swing._

/**
 * The third milestone in the project is to evolve genomes that produce green leafy plants.  The genomes
 * are scored based on how leafy their plants is.
 */
object Milestone3Main {
  def main(args: Array[String]) {
    val t = new Milestone3Main
    t.setVisible(true)
  }
}

class Milestone3Main extends JFrame with Runnable {


  import PlantUniverse._
  /** Random number generator. */
  private val rnd = new MersenneTwisterGenerateRandom

  /** The plant display. */
  private val display = new DisplayPlant

  /** The universe. */
  private val universe = new PlantUniverse
  this.universe.reset()
  this.display.universe = this.universe

  /** The population. */
  private val pop: Population = initPopulation
  /** The score function. */
  private val score = new PlantScore
  /** The genetic training. */
  private val genetic = new BasicEA(pop, score)
  this.genetic.speciation = new ArraySpeciation

  {
    this.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    genetic.addOperation(0.9, new Splice(genetic,GENOME_SIZE / 3))
    genetic.addOperation(0.1, new MutatePerturb(genetic,0.1))
    val bestGenome: DoubleArrayGenome = genetic.getBestGenome.asInstanceOf[DoubleArrayGenome]
    val physics = new PlantPhysics
    val growth = new PlantGrowth
    for (i <- 0 until 100) {
      physics.runPhysics(universe)
      growth.runGrowth(universe, bestGenome.getData)
    }
    this.getContentPane.add(this.display)
    setSize(UNIVERSE_WIDTH * 5, UNIVERSE_HEIGHT * 5)
    val t: Thread = new Thread(this)
    t.start()
  }
  // --- end of constructor ---
  /**
   * @return A random genome.
   */
  private def randomGenome: DoubleArrayGenome = {
    val genome = new DoubleArrayGenome(GENOME_SIZE)
    for(i <- 0 until genome.size) {
      genome.getData(i) = rnd.nextDouble(0, 1)
    }

    genome
  }

  /**
   * Create the initial random population.
   *
   * @return The population.
   */
  private def initPopulation: Population = {
    val result = new BasicPopulation(POPULATION_SIZE, null)
    val defaultSpecies = new BasicSpecies(result)
    for(i <- 0 until POPULATION_SIZE) {
      val genome: DoubleArrayGenome = randomGenome
      defaultSpecies.add(genome)
    }
    result.genomeFactory = new DoubleArrayGenomeFactory(GENOME_SIZE)
    result.speciesList += defaultSpecies
    result
  }

  /**
   * Perform the training iterations/generations.
   */
  override def run() {
    var generation = 0
    while (true) {
      generation += 1
      this.genetic.iteration()
      this.universe.reset()
      val bestGenome = this.genetic.getBestGenome.asInstanceOf[DoubleArrayGenome]
      val growth = new PlantGrowth
      val physics = new PlantPhysics
      for(i <- 0 until EVALUATION_CYCLES) {
        physics.runPhysics(universe)
        growth.runGrowth(universe, bestGenome.getData)
      }
      this.display.generation = generation
      this.display.bestScore = this.genetic.getBestGenome.score
      this.display.repaint()
      println(java.util.Arrays.toString(bestGenome.getLongTermMemory))
    }
  }
}