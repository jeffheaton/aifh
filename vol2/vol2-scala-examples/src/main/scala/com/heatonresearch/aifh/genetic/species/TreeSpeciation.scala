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
package com.heatonresearch.aifh.genetic.species

import com.heatonresearch.aifh.evolutionary.genome.Genome
import com.heatonresearch.aifh.evolutionary.species.ThresholdSpeciation
import com.heatonresearch.aifh.genetic.trees.TreeGenome
import com.heatonresearch.aifh.genetic.trees.TreeGenomeNode

class TreeSpeciation extends ThresholdSpeciation {
  /**
   * Compare two nodes.
   *
   * @param result The result of previous comparisons.
   * @param node1  The first node to compare.
   * @param node2  The second node to compare.
   * @return The result.
   */
  private def compareNode(result: Double, node1: TreeGenomeNode, node2: TreeGenomeNode): Double = {
    var newResult = result
    val node1Size = node1.getChildren.size
    val node2Size = node2.getChildren.size
    val childNodeCount = Math.max(node1Size, node2Size)
    for(i <- 0 until childNodeCount) {
      if (i < node1Size && i < node2Size) {
        val childNode1 = node1.getChildren.get(i)
        val childNode2 = node2.getChildren.get(i)
        newResult = compareNode(newResult, childNode1, childNode2)
      }
      else
        newResult += 1
    }
    newResult
  }

  def getCompatibilityScore(genome1: Genome, genome2: Genome): Double =
    compareNode(0, genome1.asInstanceOf[TreeGenome].root, genome2.asInstanceOf[TreeGenome].root)
}