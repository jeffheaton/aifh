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
package com.heatonresearch.aifh.learning

import com.heatonresearch.aifh.evolutionary.codec.GeneticCODEC
import com.heatonresearch.aifh.evolutionary.genome.Genome
import com.heatonresearch.aifh.genetic.genome.DoubleArrayGenome

class RBFNetworkGenomeCODEC(val inputCount : Int,val rbfCount: Int,val outputCount: Int) extends GeneticCODEC {
  def getInputCount: Int = inputCount

  def getOutputCount: Int = outputCount

  def getRbfCount: Int = rbfCount

  val size: Int = RBFNetwork(inputCount, rbfCount, outputCount).getLongTermMemory.length

  def decode(genome: Genome): MLMethod = {
    val result = RBFNetwork(inputCount, rbfCount, outputCount)
    val dag = genome.asInstanceOf[DoubleArrayGenome]
    System.arraycopy(dag.getData, 0, result.getLongTermMemory, 0, size)
    result
  }

  def encode(phenotype: MLMethod): Genome = {
    val rbfNet = phenotype.asInstanceOf[RBFNetwork]
    val result = new DoubleArrayGenome(size)
    System.arraycopy(rbfNet.getLongTermMemory, 0, result.getData, 0, size)
    result
  }
}