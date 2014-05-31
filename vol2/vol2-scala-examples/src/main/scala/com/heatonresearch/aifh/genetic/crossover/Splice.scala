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
package com.heatonresearch.aifh.genetic.crossover

import com.heatonresearch.aifh.evolutionary.genome.Genome
import com.heatonresearch.aifh.evolutionary.opp.EvolutionaryOperator
import com.heatonresearch.aifh.evolutionary.train.EvolutionaryAlgorithm
import com.heatonresearch.aifh.genetic.genome.ArrayGenome
import com.heatonresearch.aifh.randomize.GenerateRandom

/**
 * A simple cross over where genes are simply "spliced". Genes are allowed to
 * repeat.
 * @param cutLength The cut length.
 */
class Splice(owner: EvolutionaryAlgorithm,private val cutLength : Int) extends EvolutionaryOperator(owner,2,2) {

  override def performOperation(rnd: GenerateRandom, parents: Array[Genome], parentIndex: Int,
                                offspring: Array[Genome], offspringIndex: Int) {
    val mother = parents(parentIndex).asInstanceOf[ArrayGenome]
    val father = parents(parentIndex + 1).asInstanceOf[ArrayGenome]
    val offspring1 = this.owner.population.genomeFactory.factor.asInstanceOf[ArrayGenome]
    val offspring2 = this.owner.population.genomeFactory.factor.asInstanceOf[ArrayGenome]
    offspring(offspringIndex) = offspring1
    offspring(offspringIndex + 1) = offspring2
    val geneLength = mother.size
    val cutPoint1 = rnd.nextInt(geneLength - this.cutLength)
    val cutPoint2 = cutPoint1 + this.cutLength

    // handle cut section
    for(i <- 0 until geneLength) {
      if (!((i < cutPoint1) || (i > cutPoint2))) {
        offspring1.copy(father, i, i)
        offspring2.copy(mother, i, i)
      }
    }

    // handle outer sections
    for(i <- 0 until geneLength) {
      if ((i < cutPoint1) || (i > cutPoint2)) {
        offspring1.copy(mother, i, i)
        offspring2.copy(father, i, i)
      }
    }
  }
}