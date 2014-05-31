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

import com.heatonresearch.aifh.randomize.GenerateRandom

/**
 * Evaluate a tree.  Used for genetic programming.
 *
 * References:
 *
 * http://en.wikipedia.org/wiki/Reservoir_sampling
 *
 */
abstract class EvaluateTree {
  /**
   * Choose a random tree node.  Uses reservoir sampling.
   * @param rnd A random number generator.
   * @param parent The parent node.
   * @param current The current node in our traversal.
   * @param index The current index, it is an array for pass by reference.
   * @param reservoir The reservoir.
   */
  private def internalSampleRandomNode(rnd: GenerateRandom, parent: TreeGenomeNode, current: TreeGenomeNode, index: Array[Int], reservoir: RandomNodeResult) {
    val currentIndex = index(0)
    index(0) += 1
    val j: Int = rnd.nextInt(0, currentIndex + 1)
    if (j == 0) {
      reservoir.parent = parent
      reservoir.child = current
    }
    import scala.collection.JavaConversions._
    for (child <- current.getChildren) {
      internalSampleRandomNode(rnd, current, child, index, reservoir)
    }
  }

  /**
   * Choose a random node from the tree.  Uses reservoir sampling.
   * @param rnd Random number generator.
   * @param root The root of the tree.
   * @return A random node.
   */
  def sampleRandomNode(rnd: GenerateRandom, root: TreeGenomeNode): RandomNodeResult = {
    val index = new Array[Int](1)
    val reservoir = new RandomNodeResult
    index(0) = 0
    internalSampleRandomNode(rnd, null, root, index, reservoir)
    reservoir
  }

  /**
   * @return The first opcode for the variable and constant nodes.
   */
  def getVarConstOpcode: Int

  /**
   * @return The number of constants supported.
   */
  def getNumConst: Int

  /**
   * @return The number of variables supported.
   */
  def getNumVar: Int

  /**
   * Evaluate the specified node.
   * @param node The node to evaluate.
   * @param varValues The variable values.
   * @return The result of the evaluation.
   */
  def evaluate(node: TreeGenomeNode, varValues: Array[Double]): Double

  /**
   * Determine the number of children the specified opcode can have.
   * @param opcode The opcode.
   * @return The number of children this opcode can have.
   */
  def determineChildCount(opcode: Int): Int

  /**
   * @return The total number of opcodes.
   */
  def opcodeCount: Int = getVarConstOpcode + getNumVar + getNumConst

  /**
   * Choose a random opcode, choose between both leafs and nodes.
   * @param rnd A random number generator.
   * @return A random opcode.
   */
  def chooseRandomOpcode(rnd: GenerateRandom): Int = rnd.nextInt(0, opcodeCount)

  /**
   * Choose a random opcode, choose between only leafs.
   * @param rnd A random number generator.
   * @return A random opcode.
   */
  def chooseRandomLeafOpcode(rnd: GenerateRandom): Int = getVarConstOpcode + rnd.nextInt(getNumVar + getNumConst)

  /**
   * Choose a random opcode, choose between only nodes.
   * @param rnd A random number generator.
   * @return A random opcode.
   */
  def chooseRandomNodeOpcode(rnd: GenerateRandom): Int = rnd.nextInt(getVarConstOpcode)

  /**
   * Grow the tree randomly by the specified max depth.
   * @param rnd A random number generator.
   * @param maxDepth The max depth.
   * @return The tree.
   */
  def grow(rnd: GenerateRandom, maxDepth: Int): TreeGenomeNode = {
    if (maxDepth == 1) {
      new TreeGenomeNode(chooseRandomLeafOpcode(rnd))
    }
    else {
      val result = new TreeGenomeNode(chooseRandomNodeOpcode(rnd))
      val childCount: Int = determineChildCount(result.opcode)
      for(i <- 0 until childCount)
        result.getChildren.add(grow(rnd, maxDepth - 1))
      result
    }
  }

  /**
   * @return A set of leaf opcodes.
   */
  def getLeafSet: java.util.Set[Integer] = {
    val result = new java.util.HashSet[Integer]
    for(i <- this.getVarConstOpcode until opcodeCount)
      result.add(i)
    result
  }

  /**
   * @return A set of node opcodes.
   */
  def getNodeSet: java.util.Set[Integer] = {
    val result = new java.util.HashSet[Integer]
    for(i <- 0 until this.getVarConstOpcode)
      result.add(i)
    result
  }
}