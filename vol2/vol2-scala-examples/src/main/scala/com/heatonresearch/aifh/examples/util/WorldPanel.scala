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
package com.heatonresearch.aifh.examples.util

import javax.swing._
import java.awt._

/**
 * This class is used to display a grid.  This grid is often used to simulate cellular automation.
 *
 * @param rows     The number of rows in the grid.
 * @param cols     The number of columns in the grid.
 * @param showGrid True, if the grid is to be shown.
 */
class WorldPanel(rows: Int, cols: Int, val showGrid: Boolean) extends JPanel {


  /**
   * The primary, displayed grid.
   */
  private val primaryGrid = Array.ofDim[Boolean](rows, cols)
  /**
   * The backup grid.
   */
  private val backupGrid = Array.ofDim[Boolean](rows, cols)

  /**
   * @return The number of rows.
   */
  def getRows: Int = primaryGrid.length

  /**
   * @return The number of columns.
   */
  def getCols: Int = primaryGrid(0).length

  /**
   * @return The primary grid.
   */
  def getPrimaryGrid: Array[Array[Boolean]] = primaryGrid

  /**
   * @return The backup grid.
   */
  def getBackupGrid: Array[Array[Boolean]] = backupGrid

  override def paint(g: Graphics) {
    super.paint(g)
    val width: Int = this.getWidth
    val height: Int = this.getHeight
    val cellWidth: Double = width.asInstanceOf[Double] / getCols.asInstanceOf[Double]
    val cellHeight: Double = height.asInstanceOf[Double] / getRows.asInstanceOf[Double]
    g.setColor(Color.WHITE)
    g.fillRect(0, 0, width, height)
    if (this.showGrid) {
      g.setColor(Color.black)
      for(row <- 0 until getRows) {
        val y: Int = (row * cellHeight).asInstanceOf[Int]
        g.drawLine(0, y, width, y)
      }
      for(col <- 0 until getCols) {
        val x: Int = (col * cellWidth).asInstanceOf[Int]
        g.drawLine(x, 0, x, height)
      }
    }
    for(row <- 0 until getRows;
        col <- 0 until getCols) {
      val x: Int = (col * cellWidth).asInstanceOf[Int]
      val y: Int = (row * cellHeight).asInstanceOf[Int]
      if (this.primaryGrid(row)(col)) {
        g.setColor(Color.black)
        g.fillRect(x, y, cellWidth.asInstanceOf[Int], cellHeight.asInstanceOf[Int])
      }
    }
  }

  /**
   * Advance backup grid to primary.
   */
  def advanceBackupGrid() {
    for(row <- 0 until getRows) {
      System.arraycopy(this.backupGrid(row), 0, this.primaryGrid(row), 0, getCols)
    }
  }
}