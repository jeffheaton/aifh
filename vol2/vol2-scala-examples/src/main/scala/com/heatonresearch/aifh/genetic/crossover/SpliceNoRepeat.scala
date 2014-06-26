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

import com.heatonresearch.aifh.AIFHError
import com.heatonresearch.aifh.evolutionary.genome.Genome
import com.heatonresearch.aifh.evolutionary.opp.EvolutionaryOperator
import com.heatonresearch.aifh.evolutionary.train.EvolutionaryAlgorithm
import com.heatonresearch.aifh.genetic.genome.IntegerArrayGenome
import com.heatonresearch.aifh.randomize.GenerateRandom

/**
 * A simple cross over where genes are simply "spliced". Genes are not allowed
 * to repeat.  This method only works with IntegerArrayGenome.
 */
object SpliceNoRepeat {
  /**
   * Get a list of the genes that have not been taken before. This is useful
   * if you do not wish the same gene to appear more than once in a
   * genome.
   *
   * @param source The pool of genes to select from.
   * @param taken  An array of the taken genes.
   * @return Those genes in source that are not taken.
   */
  private def getNotTaken(source: IntegerArrayGenome, taken: java.util.Set[Integer]): Int = {
    for (trial <- source.getData) {
      if (!taken.contains(trial)) {
        taken.add(trial)
        return trial
      }
    }
    throw new AIFHError("Ran out of integers to select.")
  }
}

/**
 * Construct a splice crossover.
 *
 * @param cutLength The cut length.
 */
class SpliceNoRepeat(owner: EvolutionaryAlgorithm,cutLength: Int) extends EvolutionaryOperator(owner,2,2) {

  override def performOperation(rnd: GenerateRandom, parents: Array[Genome], parentIndex: Int,
                                offspring: Array[Genome], offspringIndex: Int) {
    val mother = parents(parentIndex).asInstanceOf[IntegerArrayGenome]
    val father = parents(parentIndex + 1).asInstanceOf[IntegerArrayGenome]
    val offspring1 = owner.population.genomeFactory.factor.asInstanceOf[IntegerArrayGenome]
    val offspring2 = owner.population.genomeFactory.factor.asInstanceOf[IntegerArrayGenome]
    offspring(offspringIndex) = offspring1
    offspring(offspringIndex + 1) = offspring2
    val geneLength = mother.size
    val cutPoint1 = rnd.nextInt(geneLength - this.cutLength)
    val cutPoint2 = cutPoint1 + this.cutLength
    val taken1 = new java.util.HashSet[Integer]
    val taken2 = new java.util.HashSet[Integer]
    for(i <- 0 until geneLength) {
      if (!((i < cutPoint1) || (i > cutPoint2))) {
        offspring1.copy(father, i, i)
        offspring2.copy(mother, i, i)
        taken1.add(father.getData(i))
        taken2.add(mother.getData(i))
      }
    }
    for(i <- 0 until geneLength) {
      if ((i < cutPoint1) || (i > cutPoint2)) {
        offspring1.getData(i) = SpliceNoRepeat.getNotTaken(mother, taken1)
        offspring2.getData(i) = SpliceNoRepeat.getNotTaken(father, taken2)
      }
    }
  }
}