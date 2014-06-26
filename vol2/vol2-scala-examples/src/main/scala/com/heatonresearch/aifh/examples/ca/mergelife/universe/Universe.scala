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
 * @param height  The universe height.
 * @param width   The universe width.
 * @param theSize The number of dimensions in a universe cell.
 */
class Universe(height: Int, width: Int, theSize: Int) extends Cloneable {
  /**
   * The cell size.
   */
  private val cellSize: Int = theSize
  /**
   * The universe.
   */
  private val data = Array.fill[UniverseCell](height, width)(new UniverseCell(theSize))

  /**
   * Add the specified value to the specified dimension of the specified cell.
   *
   * @param row The cell's row.
   * @param col The cell's column.
   * @param i   The index of the dimension to add to.
   * @param d   The value to add to the cell.
   */
  def add(row: Int, col: Int, i: Int, d: Double) {
    this.data(row)(col).add(i, d)
  }

  override def clone: AnyRef = {
    val result: Universe = new Universe(getHeight, getWidth, this.cellSize)
    result.copy(this)
    result
  }

  /**
   * Compare this universe to another and return the difference.  A value of zero indicates an identical universe.
   * The lower the value, the more similar.
   *
   * @param otherUniverse The other universe.
   * @return The difference between the universes.
   */
  def compare(otherUniverse: Universe): Double = {
    var result: Int = 0
    var total: Int = 0
    for(row <- 0 until otherUniverse.getHeight;
        col <- 0 until otherUniverse.getWidth) {
      val d1: Int = Math.abs((255 * get(row, col).getAvg).asInstanceOf[Int])
      val d2: Int = Math.abs((255 * otherUniverse.get(row, col).getAvg).asInstanceOf[Int])
      if (Math.abs(d1 - d2) > 10) {
        result += 1
      }
      total += 1
    }
    result.asInstanceOf[Double] / total.asInstanceOf[Double]
  }

  /**
   * Copy another universe into this one.
   *
   * @param source The source universe.
   */
  def copy(source: Universe) {
    for(row <- 0 until getHeight;
        col <- 0 until getWidth;
        i <- 0 until cellSize)
      this.data(row)(col).set(i, source.get(row, col).get(i))
  }

  /**
   * Get the universe cell for the specified row and column.
   *
   * @param row The row.
   * @param col The column.
   * @return The universe cell.
   */
  def get(row: Int, col: Int): UniverseCell = data(row)(col)

  /**
   * Get the universe cell for the specified row, column and index.
   *
   * @param row The row of the cell.
   * @param col The column of the cell.
   * @param i   The index (dimension) inside the cell.
   * @return The value.
   */
  def get(row: Int, col: Int, i: Int): Double = data(row)(col).get(i)

  /**
   * The number of dimensions inside a cell.
   *
   * @return The size of a cell.
   */
  def getCellSize: Int = cellSize

  /**
   * @return The universe grid.
   */
  def getData: Array[Array[UniverseCell]] = data

  /**
   * @return The height of the universe.
   */
  def getHeight: Int = data.length

  /**
   * @return The width of the universe.
   */
  def getWidth: Int = data(0).length

  /**
   * Determine if row and col are valid.  Both must be above zero and within the height and width.
   *
   * @param row The row.
   * @param col The column.
   * @return True, if valid.
   */
  def isValid(row: Int, col: Int): Boolean = {
    !(row < 0 || col < 0 || row >= getHeight || col >= getWidth)
  }

  /**
   * Randomize the universe.
   *
   * @param rnd A random number generator.
   */
  def randomize(rnd: GenerateRandom) {
    for(row <- 0 until getHeight ;
        col <- 0 until getWidth)
      this.data(row)(col).randomize(rnd)
  }
}