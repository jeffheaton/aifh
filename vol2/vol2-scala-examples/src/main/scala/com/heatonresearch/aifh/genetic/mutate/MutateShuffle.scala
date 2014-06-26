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
package com.heatonresearch.aifh.genetic.mutate

import com.heatonresearch.aifh.evolutionary.genome.Genome
import com.heatonresearch.aifh.evolutionary.opp.EvolutionaryOperator
import com.heatonresearch.aifh.evolutionary.train.EvolutionaryAlgorithm
import com.heatonresearch.aifh.genetic.genome.ArrayGenome
import com.heatonresearch.aifh.randomize.GenerateRandom

/**
 * A simple mutation where genes are shuffled. This mutation will not produce
 * repeated genes.
 */
class MutateShuffle(owner: EvolutionaryAlgorithm) extends EvolutionaryOperator(owner,1,1) {

  override def performOperation(rnd: GenerateRandom, parents: Array[Genome], parentIndex: Int,
                                offspring: Array[Genome], offspringIndex: Int) {
    val parent = parents(parentIndex).asInstanceOf[ArrayGenome]
    offspring(offspringIndex) = owner.population.genomeFactory.factor
    val child = offspring(offspringIndex).asInstanceOf[ArrayGenome]
    child.copy(parent)
    val length = parent.size
    var iswap1 = rnd.nextInt(length)
    var iswap2 = rnd.nextInt(length)

    // can't be equal
    if (iswap1 == iswap2) {
      // move to the next, but
      // don't go out of bounds
     if (iswap1 > 0) {
        iswap1 -= 1
      }
      else {
        iswap1 += 1
      }
    }

    // make sure they are in the right order
    if (iswap1 > iswap2) {
      val temp: Int = iswap1
      iswap1 = iswap2
      iswap2 = temp
    }
    child.swap(iswap1, iswap2)
  }
}