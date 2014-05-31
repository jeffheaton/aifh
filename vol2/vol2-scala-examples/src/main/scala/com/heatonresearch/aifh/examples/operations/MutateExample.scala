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
import com.heatonresearch.aifh.genetic.genome.DoubleArrayGenome
import com.heatonresearch.aifh.genetic.genome.DoubleArrayGenomeFactory
import com.heatonresearch.aifh.genetic.genome.IntegerArrayGenome
import com.heatonresearch.aifh.genetic.genome.IntegerArrayGenomeFactory
import com.heatonresearch.aifh.genetic.mutate.MutatePerturb
import com.heatonresearch.aifh.genetic.mutate.MutateShuffle
import com.heatonresearch.aifh.learning.MLMethod
import com.heatonresearch.aifh.learning.score.ScoreFunction
import com.heatonresearch.aifh.randomize.GenerateRandom
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom
import java.util.Arrays
import com.heatonresearch.aifh.evolutionary.genome.Genome

/**
 * This example shows how two different mutate operators create an offspring from a genome.
 * <p/>
 * Sample output from this example:
 * <p/>
 * Mutate shuffle
 * Parent: [1, 2, 3, 4, 5]
 * Offspring: [1, 3, 2, 4, 5]
 * Mutate peterb
 * Parent: [1.0, 2.0, 3.0, 4.0, 5.0]
 * Offspring: [0.9684564148017776, 2.0231188741090955, 3.200690276405833, 4.050125858385886, 4.531099177190473]
 */
object MutateExample {
  /**
   * Demonstrate the mutate shuffle operator.  An offspring will be created by swapping two
   * individual genes.
   */
  def mutateShuffle() {
    println("Mutate shuffle")
    val rnd: GenerateRandom = new MersenneTwisterGenerateRandom
    val pop: Population = new BasicPopulation
    pop.genomeFactory = new IntegerArrayGenomeFactory(5)
    val train: EvolutionaryAlgorithm = new BasicEA(pop, new ScoreFunction {
      def calculateScore(method: MLMethod): Double = 0
      def shouldMinimize: Boolean = false
    })
    val opp = new MutateShuffle(train)
    train.addOperation(1.0, opp)
    val parents = new Array[IntegerArrayGenome](1)
    parents(0) = pop.genomeFactory.factor.asInstanceOf[IntegerArrayGenome]
    for(i <- 1 to 5)
      parents(0).getData(i - 1) = i
    val offspring = new Array[IntegerArrayGenome](1)
    offspring(0) = new IntegerArrayGenome(5)
    opp.performOperation(rnd, parents.asInstanceOf[Array[Genome]], 0, offspring.asInstanceOf[Array[Genome]], 0)
    println("Parent: " + Arrays.toString(parents(0).getData))
    println("Offspring: " + Arrays.toString(offspring(0).getData))
  }

  /**
   * Demonstrate the mutate peterb operator.  An offspring will be created by randomly changing each
   * gene.
   */
  def mutatePeterb() {
    println("Mutate peterb")
    val rnd: GenerateRandom = new MersenneTwisterGenerateRandom
    val pop: Population = new BasicPopulation
    pop.genomeFactory = new DoubleArrayGenomeFactory(5)
    val train: EvolutionaryAlgorithm = new BasicEA(pop, new ScoreFunction {
      def calculateScore(method: MLMethod): Double = 0

      def shouldMinimize: Boolean = false
    })
    val opp = new MutatePerturb(train,0.1)
    train.addOperation(1.0, opp)
    val parents = new Array[DoubleArrayGenome](1)
    parents(0) = pop.genomeFactory.factor.asInstanceOf[DoubleArrayGenome]
    parents(0).population = pop
    for(i <- 1 to 5)
      parents(0).getData(i - 1) = i
    val offspring = new Array[DoubleArrayGenome](1)
    offspring(0) = new DoubleArrayGenome(5)
    opp.performOperation(rnd, parents.asInstanceOf[Array[Genome]], 0, offspring.asInstanceOf[Array[Genome]], 0)
    println("Parent: " + Arrays.toString(parents(0).getData))
    println("Offspring: " + Arrays.toString(offspring(0).getData))
  }

  /**
   * Main entry point for the program.
   *
   * @param args Not used.
   */
  def main(args: Array[String]) {
    mutateShuffle()
    mutatePeterb()
  }
}