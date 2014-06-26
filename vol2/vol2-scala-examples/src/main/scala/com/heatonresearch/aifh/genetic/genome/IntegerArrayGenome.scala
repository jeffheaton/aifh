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
 * A genome that is an array of discrete integer values.
 */
@SerialVersionUID(1L)
class IntegerArrayGenome(var data: Array[Int]) extends BasicGenome with ArrayGenome {
  /**
   * Construct the genome.
   *
   * @param size The size of the genome.
   */
  def this(size : Int) {
    this(new Array[Int](size))
  }

  /**
   * Construct the genome by copying another.
   *
   * @param other The other genome.
   */
  def this(other: IntegerArrayGenome) {
    this(other.getData.clone())
  }

  override def size: Int = data.length

  override def copy(source: ArrayGenome, sourceIndex: Int, targetIndex: Int) {
    val sourceInt = source.asInstanceOf[IntegerArrayGenome]
    this.data(targetIndex) = sourceInt.data(sourceIndex)
  }

  def getData: Array[Int] = data

  override def copy(source: Genome) {
    val sourceInt = source.asInstanceOf[IntegerArrayGenome]
    System.arraycopy(sourceInt.data, 0, this.data, 0, this.data.length)
    this.score = source.score
    this.adjustedScore = source.adjustedScore
  }

  override def swap(iswap1: Int, iswap2: Int) {
    val temp: Int = this.data(iswap1)
    this.data(iswap1) = this.data(iswap2)
    this.data(iswap2) = temp
  }

  /**
   * Not supported.  Integer files are not generally used for model fitting.
   *
   * @return Nothing.
   */
  def getLongTermMemory: Array[Double] = {
    throw new UnsupportedOperationException
  }
}