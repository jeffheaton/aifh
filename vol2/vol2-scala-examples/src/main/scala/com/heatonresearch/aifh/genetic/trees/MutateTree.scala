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
package com.heatonresearch.aifh.genetic.trees

import com.heatonresearch.aifh.evolutionary.genome.Genome
import com.heatonresearch.aifh.evolutionary.opp.EvolutionaryOperator
import com.heatonresearch.aifh.evolutionary.train.EvolutionaryAlgorithm
import com.heatonresearch.aifh.randomize.GenerateRandom

class MutateTree(owner: EvolutionaryAlgorithm,theMaxGraftLength: Int) extends EvolutionaryOperator(owner,1,1) {

  private val maxGraftLength = Math.max(1, theMaxGraftLength)

  override def performOperation(rnd: GenerateRandom, parents: Array[Genome], parentIndex: Int,
                                offspring: Array[Genome], offspringIndex: Int) {
    val parent1 = parents(parentIndex).asInstanceOf[TreeGenome]
    val eval = parent1.evaluator
    val off1 = owner.population.genomeFactory.factor(parent1).asInstanceOf[TreeGenome]
    val off1Point = eval.sampleRandomNode(rnd, off1.root)
    val len = rnd.nextInt(1, this.maxGraftLength + 1)
    val randomSequence = eval.grow(rnd, len)
    if (off1Point.parent == null) {
      off1.root = randomSequence
    }
    else {
      val idx = off1Point.parent.getChildren.indexOf(off1Point.child)
      off1Point.parent.getChildren.set(idx, randomSequence)
    }

    // TODO is this right - offspringIndex appears to be unused
    offspring(0) = off1
  }
}