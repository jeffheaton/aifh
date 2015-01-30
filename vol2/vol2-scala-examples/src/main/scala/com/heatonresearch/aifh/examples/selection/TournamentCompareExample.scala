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
package com.heatonresearch.aifh.examples.selection

import com.heatonresearch.aifh.evolutionary.genome.Genome
import com.heatonresearch.aifh.evolutionary.opp.selection.TournamentSelection
import com.heatonresearch.aifh.evolutionary.population.BasicPopulation
import com.heatonresearch.aifh.evolutionary.train.EvolutionaryAlgorithm
import com.heatonresearch.aifh.evolutionary.train.basic.BasicEA
import com.heatonresearch.aifh.genetic.genome.IntegerArrayGenome
import com.heatonresearch.aifh.learning.MLMethod
import com.heatonresearch.aifh.learning.score.ScoreFunction
import com.heatonresearch.aifh.randomize.GenerateRandom
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom

/**
 * This example shows how the number of rounds affects the average score of the genome selected by
 * the tournament selection operator.  A population of 1000 genomes is created with each genome having a
 * score between 0 and 999.  There is one genome for each score.  Round counts are tried between one and ten.
 * The average score over 100k selections is reported.  As the number of rounds increases, so does the average
 * score selected.
 * <p/>
 * Sample output is shown here:
 * <p/>
 * Rounds: 1, Avg Score: 665
 * Rounds: 2, Avg Score: 749
 * Rounds: 3, Avg Score: 800
 * Rounds: 4, Avg Score: 833
 * Rounds: 5, Avg Score: 856
 * Rounds: 6, Avg Score: 874
 * Rounds: 7, Avg Score: 888
 * Rounds: 8, Avg Score: 899
 * Rounds: 9, Avg Score: 908
 * Rounds: 10, Avg Score: 915
 */
object TournamentCompareExample {
  def main(args: Array[String]) {
    val pop = new BasicPopulation
    val species = pop.createSpecies
    for(i <- 0 until 1000) {
      val genome: Genome = new IntegerArrayGenome(1)
      genome.score = i
      genome.adjustedScore = i
      pop.speciesList(0).add(genome)
    }

    val rnd: GenerateRandom = new MersenneTwisterGenerateRandom
    val train: EvolutionaryAlgorithm = new BasicEA(pop, new ScoreFunction {
      def calculateScore(method: MLMethod): Double = 0

      def shouldMinimize: Boolean = false
    })
    for(roundCount <- 1 to 10) {
      val selection = new TournamentSelection(train, roundCount)
      var sum = 0
      var count = 0
      for(i <- 0 until 100000) {
        val genomeID = selection.performSelection(rnd, species)
        val genome = species.members(genomeID)
        sum += genome.adjustedScore.toInt
        count += 1
      }
      sum /= count
      println("Rounds: " + roundCount + ", Avg Score: " + sum)
    }
  }
}