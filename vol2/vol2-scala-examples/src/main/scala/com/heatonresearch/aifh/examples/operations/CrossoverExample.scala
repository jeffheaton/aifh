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
package com.heatonresearch.aifh.examples.operations

import com.heatonresearch.aifh.evolutionary.population.BasicPopulation
import com.heatonresearch.aifh.evolutionary.population.Population
import com.heatonresearch.aifh.evolutionary.train.EvolutionaryAlgorithm
import com.heatonresearch.aifh.evolutionary.train.basic.BasicEA
import com.heatonresearch.aifh.genetic.crossover.Splice
import com.heatonresearch.aifh.genetic.crossover.SpliceNoRepeat
import com.heatonresearch.aifh.genetic.genome.IntegerArrayGenome
import com.heatonresearch.aifh.genetic.genome.IntegerArrayGenomeFactory
import com.heatonresearch.aifh.learning.MLMethod
import com.heatonresearch.aifh.learning.score.ScoreFunction
import com.heatonresearch.aifh.randomize.GenerateRandom
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom
import com.heatonresearch.aifh.evolutionary.genome.Genome
import java.util

/**
 * This example demonstrates crossover using a splice operator.  Both the repeat and non-repeat
 * variants are shown.
 * <p/>
 * Sample output:
 * <p/>
 * Crossover Splice
 * Parent 1: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
 * Parent 2: [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
 * Offspring 1: [1, 2, 3, 7, 6, 5, 4, 3, 2, 10]
 * Offspring 2: [10, 9, 8, 4, 5, 6, 7, 8, 9, 1]
 * Crossover Splice No Repeat
 * Parent 1: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
 * Parent 2: [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
 * Offspring 1: [1, 8, 9, 7, 6, 5, 4, 3, 2, 10]
 * Offspring 2: [10, 3, 2, 4, 5, 6, 7, 8, 9, 1]
 */
object CrossoverExample {
  /**
   * Demonstrate the crossover splice operator.  Two offspring will be created by swapping three
   * segments of the parents (two cut points). Some genes may repeat.
   */
  def splice() {
    println("Crossover Splice")
    val rnd: GenerateRandom = new MersenneTwisterGenerateRandom
    val pop: Population = new BasicPopulation
    pop.genomeFactory = new IntegerArrayGenomeFactory(10)
    val train: EvolutionaryAlgorithm = new BasicEA(pop, new ScoreFunction {
      def calculateScore(method: MLMethod): Double = 0

      def shouldMinimize: Boolean = false
    })
    val opp: Splice = new Splice(train,5)
    train.addOperation(1.0, opp)
    val parents = Array[IntegerArrayGenome](
      pop.genomeFactory.factor.asInstanceOf[IntegerArrayGenome],
      pop.genomeFactory.factor.asInstanceOf[IntegerArrayGenome])

    for(i <- 1 to 10) {
      parents(0).getData(i - 1) = i
      parents(1).getData(i - 1) = 11 - i
    }
    val offspring: Array[IntegerArrayGenome] = new Array[IntegerArrayGenome](2)
    opp.performOperation(rnd, parents.asInstanceOf[Array[Genome]], 0, offspring.asInstanceOf[Array[Genome]], 0)
    println("Parent 1: " + util.Arrays.toString(parents(0).getData))
    println("Parent 2: " + util.Arrays.toString(parents(1).getData))
    println("Offspring 1: " + util.Arrays.toString(offspring(0).getData))
    println("Offspring 2: " + util.Arrays.toString(offspring(1).getData))
  }

  /**
   * Demonstrate the crossover splice operator (no repeat).  Two offspring will be created by
   * swapping three segments of the parents (two cut points). No repeated genes allowed per offspring.
   */
  def spliceNoRepeat() {
    println("Crossover Splice No Repeat")
    val rnd: GenerateRandom = new MersenneTwisterGenerateRandom
    val pop: Population = new BasicPopulation
    pop.genomeFactory = new IntegerArrayGenomeFactory(10)
    val train: EvolutionaryAlgorithm = new BasicEA(pop, new ScoreFunction {
      def calculateScore(method: MLMethod): Double = 0

      def shouldMinimize: Boolean = false
    })
    val opp: SpliceNoRepeat = new SpliceNoRepeat(train,5)
    train.addOperation(1.0, opp)
    val parents = Array[IntegerArrayGenome](
      pop.genomeFactory.factor.asInstanceOf[IntegerArrayGenome],
      pop.genomeFactory.factor.asInstanceOf[IntegerArrayGenome])
    for(i <- 1 to 10) {
      parents(0).getData(i - 1) = i
      parents(1).getData(i - 1) = 11 - i
    }
    val offspring: Array[IntegerArrayGenome] = new Array[IntegerArrayGenome](2)
    opp.performOperation(rnd, parents.asInstanceOf[Array[Genome]], 0, offspring.asInstanceOf[Array[Genome]], 0)
    println("Parent 1: " + util.Arrays.toString(parents(0).getData))
    println("Parent 2: " + util.Arrays.toString(parents(1).getData))
    println("Offspring 1: " + util.Arrays.toString(offspring(0).getData))
    println("Offspring 2: " + util.Arrays.toString(offspring(1).getData))
  }

  /**
   * Main entry point.
   *
   * @param args Not used.
   */
  def main(args: Array[String]) {
    splice()
    spliceNoRepeat()
  }
}