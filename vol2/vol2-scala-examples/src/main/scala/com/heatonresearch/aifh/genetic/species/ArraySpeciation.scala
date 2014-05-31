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

import com.heatonresearch.aifh.AIFHError
import com.heatonresearch.aifh.evolutionary.genome.Genome
import com.heatonresearch.aifh.evolutionary.species.ThresholdSpeciation
import com.heatonresearch.aifh.genetic.genome.DoubleArrayGenome
import com.heatonresearch.aifh.genetic.genome.IntegerArrayGenome

class ArraySpeciation extends ThresholdSpeciation {
  def getCompatibilityScore(genome1: Genome, genome2: Genome): Double = {
    genome1 match {
      case _: DoubleArrayGenome =>
        scoreDouble(genome1, genome2)
      case _: IntegerArrayGenome =>
        scoreInt(genome1, genome2)
      case _ =>
        throw new AIFHError("This speciation does not support: " + genome1.getClass.getName)
    }
  }

  private def scoreInt(genome1: Genome, genome2: Genome): Double = {
    val intGenome1: IntegerArrayGenome = genome1.asInstanceOf[IntegerArrayGenome]
    val intGenome2: IntegerArrayGenome = genome2.asInstanceOf[IntegerArrayGenome]
    var sum: Double = 0
    for(i <- 0 until intGenome1.size) {
      val diff: Double = intGenome1.getData(i) - intGenome2.getData(i)
      sum += diff * diff
    }

    Math.sqrt(sum)
  }

  private def scoreDouble(genome1: Genome, genome2: Genome): Double = {
    val doubleGenome1: DoubleArrayGenome = genome1.asInstanceOf[DoubleArrayGenome]
    val doubleGenome2: DoubleArrayGenome = genome2.asInstanceOf[DoubleArrayGenome]
    var sum: Double = 0
    for(i <- 0 until doubleGenome1.size) {
      val diff: Double = doubleGenome1.getData(i) - doubleGenome2.getData(i)
      sum += diff * diff
    }
    Math.sqrt(sum)
  }
}