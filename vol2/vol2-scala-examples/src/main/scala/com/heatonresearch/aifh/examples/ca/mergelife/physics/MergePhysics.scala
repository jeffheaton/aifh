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
package com.heatonresearch.aifh.examples.ca.mergelife.physics

import com.heatonresearch.aifh.examples.ca.mergelife.universe.Universe
import com.heatonresearch.aifh.examples.ca.mergelife.universe.UniverseCell
import java.io._
import java.util.StringTokenizer
import java.util

/**
 * Merge physics determines how cells are to be populated.
 * <p/>
 * For more information on how the physics works see:
 * <p/>
 * http://www.codeproject.com/Articles/730362/Using-an-evolutionary-algorithm-to-create-a-cellul
 * @param universe The universe.
 */
class MergePhysics(private val universe: Universe) extends Physics {
  /**
   * The color table.
   */
  private val colorTable: Array[Array[Double]] = Array(
    Array(-1.0, -1.0, -1.0), Array(1.0, -1.0, -1.0),
    Array(-1.0, 1.0, -1.0), Array(1.0, 1.0, -1.0),
    Array(-1.0, -1.0, 1.0), Array(1.0, -1.0, 1.0),
    Array(-1.0, 1.0, 1.0), Array(1.0, 1.0, 1.0))
  /**
   * Transformations to move from a cell to the 9 neighboring cells.
   * These are the column values.
   */
  private val colTransform: Array[Int] = Array(0, 0, -1, 1, -1, 1, 1, -1)
  /**
   * Transformations to move from a cell to the 9 neighboring cells.
   * These are the row values.
   */
  private val rowTransform: Array[Int] = Array(-1, 1, 0, 0, -1, 1, -1, 1)
  /**
   * The data that defines the physical constants.
   */
  private val data = new Array[Double](2 * colorTable.length)
  /**
   * The data sorted.
   */
  private val dataOrder = new Array[Int](colorTable.length)


  override def copyData(sourceData: Array[Double]) {
    System.arraycopy(sourceData, 0, data, 0, data.length)
    sortData()
  }

  override def getData: Array[Double] = data

  /**
   * Determine if "value" is in the array.
   *
   * @param list  The list.
   * @param len   The length.
   * @param value The value.
   * @return True, if value is in the list.
   */
  private def listContains(list: Array[Int], len: Int, value: Int): Boolean = {
    for(i <- 0 until len) {
      if (list(i) == value) {
        return true
      }
    }
    false
  }

  override def load(filename: String) {
    val br: BufferedReader = new BufferedReader(new FileReader(filename))
    try {
      var line: String = br.readLine
      line = line.substring(1)
      line = line.substring(0, line.length - 1)
      val tok: StringTokenizer = new StringTokenizer(line, ",c")
      var idx: Int = 0
      while (tok.hasMoreElements) {
        data(idx) = tok.nextToken.toDouble
        idx += 1
        sortData()
      }
    }
    finally {
      br.close()
    }
  }

  override def processPixel(outputUniverse: Universe, row: Int, col: Int) {
    var total: Double = 0
    var cnt: Int = 0
    for(dir <- 0 until rowTransform.length) {
      val otherRow: Int = row + rowTransform(dir)
      val otherCol: Int = col + colTransform(dir)
      if (universe.isValid(otherRow, otherCol)) {
        val otherCell: UniverseCell = universe.get(otherRow, otherCol)
        total += otherCell.getAvg
        cnt += 1
      }
    }

    class BreakException extends Exception

    try {
      total /= cnt
      for(i <- 0 until colorTable.length) {
        val idx = dataOrder(i)
        if (total < data(idx * 2)) {
          for(j <- 0 until outputUniverse.getCellSize) {
            var d: Double = colorTable(idx)(j) - universe.get(row, col, j)
            var pct = data(1 + idx * 2)
            pct = (pct + 1.0) / 2.0
            d *= pct
            outputUniverse.add(row, col, j, d)
          }
          throw new BreakException
        }
      }
    } catch {
      case _ : BreakException =>
    }
  }

  def randomize() {
    for(i <- 0 until data.length)
      data(i) = Math.random * 2.0 - 1.0
    sortData()
  }

  @throws[IOException]
  override def save(filename: String) {
    val out = new BufferedWriter(new FileWriter(filename))
    out.write(util.Arrays.toString(data))
    out.newLine()
    out.close()
  }

  /**
   * Sort the physical data for faster lookup.
   */
  private def sortData() {
    for(x <- 0 until colorTable.length) {
      var lowestIndex = -1
      for(y <- 0 until colorTable.length) {
        val value: Double = data(y * 2)
        if (!listContains(dataOrder, x, y)) {
          if (lowestIndex == -1) {
            lowestIndex = y
          }
          else {
            if (value < data(lowestIndex * 2))
              lowestIndex = y
          }
        }
      }
      dataOrder(x) = lowestIndex
    }
  }
}