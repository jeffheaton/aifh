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
package com.heatonresearch.aifh.genetic.genome

import com.heatonresearch.aifh.evolutionary.genome.BasicGenome
import com.heatonresearch.aifh.evolutionary.genome.Genome

/**
 * A genome made up of continuous doubles.
 */
@SerialVersionUID(1L)
class DoubleArrayGenome(private val data: Array[Double]) extends BasicGenome with ArrayGenome {

  def this(sizeVal : Int) {
    this(new Array[Double](sizeVal))
  }

  /**
   * Construct a genome based on another genome.
   *
   * @param other The other genome.
   */
  def this(other: DoubleArrayGenome) {
    this(other.getData.clone())
  }

  def size: Int = data.length

  def copy(source: ArrayGenome, sourceIndex: Int, targetIndex: Int) {
    val sourceInt: DoubleArrayGenome = source.asInstanceOf[DoubleArrayGenome]
    this.data(targetIndex) = sourceInt.data(sourceIndex)
  }

  /**
   * @return The data.
   */
  def getData: Array[Double] = data

  def getLongTermMemory: Array[Double] = data

  def copy(source: Genome) {
    val sourceDouble: DoubleArrayGenome = source.asInstanceOf[DoubleArrayGenome]
    System.arraycopy(sourceDouble.data, 0, this.data, 0, this.data.length)
    score = source.score
    adjustedScore = source.adjustedScore
  }

  def swap(iswap1: Int, iswap2: Int) {
    val temp: Double = this.data(iswap1)
    this.data(iswap1) = this.data(iswap2)
    this.data(iswap2) = temp
  }
}