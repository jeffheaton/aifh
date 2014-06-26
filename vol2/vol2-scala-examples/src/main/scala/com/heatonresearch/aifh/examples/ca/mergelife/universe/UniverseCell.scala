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
package com.heatonresearch.aifh.examples.ca.mergelife.universe

import com.heatonresearch.aifh.randomize.GenerateRandom

/**
 * An individual universe cell.  Each cell can have multiple properties.  Typically there are three properties,
 * that represent the three RGB components.
 * @param propsSize The number of properties.
 */
class UniverseCell(val propsSize : Int) {

  /**
   * The properties for this cell.
   */
  private val prop = new Array[Double](propsSize)

  /**
   * Add the specified value to the specified property.
   *
   * @param i The property index.
   * @param d The other value to add.
   */
  def add(i: Int, d: Double) {
    prop(i) += d
  }

  /**
   * Add the properties of another cell to this one.
   *
   * @param otherCell The other cell.
   */
  def add(otherCell: UniverseCell) {
    for(i <- 0 until prop.length)
      prop(i) += otherCell.get(i)
  }

  /**
   * Get a property.
   *
   * @param i The index of the property.
   * @return The property value.
   */
  def get(i: Int): Double = prop(i)

  /**
   * Get an average of the properties.
   *
   * @return The propety average.
   */
  def getAvg: Double = prop.sum / prop.length

  /**
   * @return The property array.
   */
  def getData: Array[Double] = this.prop

  /**
   * Randomize the properties between (-1,1).
   *
   * @param rnd A random number generator.
   */
  def randomize(rnd: GenerateRandom) {
    for(i <- 0 until prop.length) {
      prop(i) = rnd.nextDouble(-1, 1)
    }
  }

  /**
   * Set the specified property.
   *
   * @param i The index of the property.
   * @param d The property value.
   */
  def set(i: Int, d: Double) {
    prop(i) = d
  }

  /**
   * Set this cell's properties to another cell.
   *
   * @param otherCell The other cell.
   */
  def set(otherCell: UniverseCell) {
    for(i <- 0 until prop.length) {
      prop(i) = otherCell.get(i)
    }
  }

  /**
   * @return The number of properties.
   */
  def size: Int = prop.length
}