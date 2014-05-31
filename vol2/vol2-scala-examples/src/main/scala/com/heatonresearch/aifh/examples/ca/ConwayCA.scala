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
package com.heatonresearch.aifh.examples.ca

import com.heatonresearch.aifh.examples.util.WorldPanel
import com.heatonresearch.aifh.randomize.GenerateRandom
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom
import javax.swing._
import java.awt._
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.awt.event.WindowEvent
import java.awt.event.WindowListener

/**
 * Conway's game of life.  A classic cellular automation.
 * <p/>
 * Rules:
 * 1. Any live cell with fewer than two live neighbors dies, as if caused by under-population.
 * 2. Any live cell with two or three live neighbors lives on to the next generation. (not needed)
 * 3. Any live cell with more than three live neighbors dies, as if by overcrowding.
 * 4. Any dead cell with exactly three live neighbors becomes a live cell, as if by reproduction.
 * <p/>
 * References:
 * http://en.wikipedia.org/wiki/Conway's_Game_of_Life
 */
object ConwayCA {
  /**
   * Main entry point.
   *
   * @param args Not used.
   */
  def main(args: Array[String]) {
    try {
      val f: JFrame = new ConwayCA
      f.setVisible(true)
    }
    catch {
      case ex: Exception =>
        ex.printStackTrace()
    }
  }

  /**
   * Used to locate the x coordinate of neighbors.
   */
  private val neighborsX: Array[Int] = Array(0, 0, 1, -1, -1, 1, -1, 1)
  /**
   * Used to locate the y coordinate of neighbors.
   */
  private val neighborsY: Array[Int] = Array(1, -1, 0, 0, -1, -1, 1, 1)
  /**
   * The number of rows.
   */
  val ROWS: Int = 75
  /**
   * The number of columns.
   */
  val COLS: Int = 75
}

class ConwayCA extends JFrame with ActionListener with WindowListener with Runnable {
  import ConwayCA._

  /** Perform an iteration. */
  private val iterationButton: JButton = new JButton("Iteration")
  /** Start running. */
  private val startButton: JButton = new JButton("Start")
  /** Stop running. */
  private val stopButton: JButton = new JButton("Stop")
  /** Reset the simulation. */
  private val resetButton: JButton = new JButton("Reset")
  /** Has a stop been requested. */
  private var requestStop: Boolean = false
  /** The world. */
  private var worldArea: WorldPanel = null

  {
    setSize(500, 500)
    setTitle("Conway's Game of Life")
    val c: Container = getContentPane
    c.setLayout(new BorderLayout)
    val buttonPanel: JPanel = new JPanel
    buttonPanel.setLayout(new FlowLayout(FlowLayout.LEFT))
    c.add(buttonPanel, BorderLayout.NORTH)
    buttonPanel.add(iterationButton)
    buttonPanel.add(startButton)
    buttonPanel.add(stopButton)
    buttonPanel.add(resetButton)
    worldArea = new WorldPanel(ROWS, COLS, true)
    val scroll: JScrollPane = new JScrollPane(worldArea)
    c.add(scroll, BorderLayout.CENTER)
    iterationButton.addActionListener(this)
    startButton.addActionListener(this)
    stopButton.addActionListener(this)
    resetButton.addActionListener(this)
    addWindowListener(this)
    stopButton.setEnabled(false)
    performReset()
  }

  /**
   * Perform one iteration.
   */
  def performIteration() {
    val grid: Array[Array[Boolean]] = worldArea.getPrimaryGrid
    for(row <- 0 until grid.length) {
      for (col <- 0 until grid(row).length) {
        var total: Int = 0
        for (i <- 0 until neighborsX.length) {
          val nCol: Int = col + neighborsX(i)
          val nRow: Int = row + neighborsY(i)
          if (nCol >= 0 && nCol < worldArea.getCols) {
            if (nRow >= 0 && nRow < worldArea.getRows) {
              if (grid(nRow)(nCol)) {
                total += 1
              }
            }
          }
        }
        var alive: Boolean = grid(row)(col)
        if (alive) {
          if (total < 2) {
            alive = false
          }
          if (alive && total > 3) {
            alive = false
          }
        }
        else {
          if (total == 3) {
            alive = true
          }
        }
        worldArea.getBackupGrid(row)(col) = alive
      }
    }
    worldArea.advanceBackupGrid()
  }

  /**
   * Start the simulation.
   */
  def performStart() {
    iterationButton.setEnabled(false)
    stopButton.setEnabled(true)
    startButton.setEnabled(false)
    val thread: Thread = new Thread(this)
    thread.start()
  }

  /**
   * Request stop.
   */
  def performStop() {
    requestStop = true
  }

  /**
   * Perform a reset.
   */
  def performReset() {
    val grid: Array[Array[Boolean]] = worldArea.getBackupGrid
    val rnd: GenerateRandom = new MersenneTwisterGenerateRandom
    for(row <- 0 until worldArea.getRows;
        col <- 0 until worldArea.getCols) {
      grid(row)(col) = rnd.nextBoolean
    }
    worldArea.advanceBackupGrid()
  }

  override def actionPerformed(ev: ActionEvent) {
    if (ev.getSource eq iterationButton) {
      performIteration()
      repaint()
    }
    else if (ev.getSource eq startButton) {
      performStart()
    }
    else if (ev.getSource eq stopButton) {
      performStop()
    }
    else if (ev.getSource eq resetButton) {
      performReset()
    }
  }

  override def windowActivated(arg0: WindowEvent) {}

  override def windowClosed(arg0: WindowEvent) {}

  override def windowClosing(arg0: WindowEvent) {
    System.exit(0)
  }

  override def windowDeactivated(arg0: WindowEvent) {}

  override def windowDeiconified(arg0: WindowEvent) {}

  override def windowIconified(arg0: WindowEvent) {}

  override def windowOpened(arg0: WindowEvent) {
    performReset()
  }

  override def run() {
    requestStop = false
    while (!requestStop) {
      performIteration()
      repaint()
      try {
        Thread.sleep(100)
      } catch {
        case ex: InterruptedException =>
          ex.printStackTrace()
      }
    }
    iterationButton.setEnabled(true)
    stopButton.setEnabled(false)
    startButton.setEnabled(true)
  }
}