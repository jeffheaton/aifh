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

import com.heatonresearch.aifh.AIFHError
import com.heatonresearch.aifh.evolutionary.genome.BasicGenome
import com.heatonresearch.aifh.evolutionary.genome.Genome
import com.heatonresearch.aifh.learning.RegressionAlgorithm

class TreeGenome(val evaluator: EvaluateTree) extends BasicGenome with RegressionAlgorithm {

  var root: TreeGenomeNode = null

  override def copy(source: Genome) {
    this.root = source.asInstanceOf[TreeGenome].root.copy
  }

  override def size: Int = root.size

  override def getLongTermMemory: Array[Double] = {
    throw new AIFHError("Long term memory not supported, use a genetic trainer.")
  }

  override def computeRegression(input: Array[Double]): Array[Double] = {
    val result: Array[Double] = new Array[Double](1)
    result(0) = evaluator.evaluate(this.root, input)
    result
  }
}