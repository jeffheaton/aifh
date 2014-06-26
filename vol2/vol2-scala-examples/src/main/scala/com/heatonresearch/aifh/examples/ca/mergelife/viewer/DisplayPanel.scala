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
package com.heatonresearch.aifh.examples.ca.mergelife.viewer

import com.heatonresearch.aifh.examples.ca.mergelife.physics.Physics
import com.heatonresearch.aifh.examples.ca.mergelife.universe.AdvanceTask
import com.heatonresearch.aifh.examples.ca.mergelife.universe.UniverseRunner
import com.heatonresearch.aifh.randomize.GenerateRandom
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom
import javax.swing._
import java.awt._
import java.awt.event.MouseEvent
import java.awt.event.MouseListener
import java.awt.image.BufferedImage
import java.io.IOException
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

/**
* Display panel for a universe.
 *
 * @param owner The parent frame.
 * @param rows  The number of rows.
 * @param cols  The number of columns.
*/
class DisplayPanel(val owner: MultiverseViewer, val rows: Int, val cols: Int) extends JPanel with MouseListener {

  /**
   * The multiverse grid.
   */
  private val grid: Array[Array[UniversePane]] = Array.fill(rows,cols)(new UniversePane)

  /**
   * The thread pool.
   */
  private val threadPool: ExecutorService = Executors.newFixedThreadPool(8)

  /**
   * Random number generator.
   */
  private val rnd: GenerateRandom = new MersenneTwisterGenerateRandom

  /**
   * The offscreen image, this reduces flicker.
   */
  private val offscreenImage: BufferedImage = new BufferedImage(this.cols * MultiverseViewer.getConfig.paneWidth,
    this.rows * MultiverseViewer.getConfig.paneHeight, BufferedImage.TYPE_INT_ARGB)

  /**
   * A graphics device for off-screen rendering.
   */
  private val offscreenGraphics: Graphics = offscreenImage.createGraphics

  addMouseListener(this)
  // end of construction ---

  /**
   * Begin a copy.
   *
   * @param row The multiverse row.
   * @param col The multiverse column.
   */
  def copyPane(row: Int, col: Int) {
    this.copySource = this.grid(row)(col).getUniverseRunner
    this.crossoverParent1 = null
    this.crossoverParent2 = null
    this.owner.enableDeselect()
  }

  /**
   * Identify the first parent for a crossover.
   *
   * @param row The multiverse row.
   * @param col The multiverse column.
   */
  def crossover(row: Int, col: Int) {
    this.crossoverParent1 = this.grid(row)(col).getUniverseRunner
    this.crossoverParent2 = null
    this.copySource = null
    this.owner.enableDeselect()
  }

  /**
   * Deselect any selected universes.
   */
  def deselect() {
    this.copySource = null
    this.crossoverParent1 = null
    this.crossoverParent2 = null
  }

  /**
   * Show the popup menu.
   *
   * @param e The mouse event.
   */
  private def doPop(e: MouseEvent) {
    val x: Int = e.getX
    val y: Int = e.getY
    val row: Int = y / MultiverseViewer.getConfig.paneHeight
    val col: Int = x / MultiverseViewer.getConfig.paneWidth
    val menu: PanePopup = new PanePopup(this, row, col)
    menu.show(e.getComponent, e.getX, e.getY)
  }

  /**
   * Draw the status for a universe.
   *
   * @param g   Graphics device.
   * @param row The multiverse row.
   * @param col The multiverse column.
   * @param fm  The font metrics.
   * @param s The text.
   */
  private def drawStatus(g: Graphics, row: Int, col: Int, fm: FontMetrics, s: String) {
    val x: Int = col * MultiverseViewer.getConfig.paneWidth
    val y: Int = row * MultiverseViewer.getConfig.paneHeight
    val textY: Int = y + MultiverseViewer.getConfig.paneHeight
    g.setColor(Color.LIGHT_GRAY)
    g.fillRect(x, textY - fm.getHeight, MultiverseViewer.getConfig.paneWidth, fm.getHeight)
    g.setColor(Color.BLACK)
    g.drawString(s, x, textY)
  }

  /**
   * Load a universe.
   *
   * @param row The multiverse row.
   * @param col The multiverse column.
   */
  def load(row: Int, col: Int) {
    try {
      val fc: JFileChooser = new JFileChooser
      fc.setCurrentDirectory(MultiverseViewer.getConfig.saveDirectory)
      val returnVal: Int = fc.showOpenDialog(this)
      if (returnVal == JFileChooser.APPROVE_OPTION) {
        MultiverseViewer.getConfig.saveDirectory = fc.getSelectedFile.getParentFile
        this.grid(row)(col).getUniverseRunner.physics.load(fc.getSelectedFile.toString)
        this.grid(row)(col).getUniverseRunner.randomize(this.rnd)
      }
    }
    catch {
      case ex: IOException =>
        ex.printStackTrace()
    }
  }

  override def mouseClicked(e: MouseEvent) {
  }

  override def mouseEntered(e: MouseEvent) {
  }

  override def mouseExited(e: MouseEvent) {
  }

  override def mousePressed(e: MouseEvent) {
    if (e.isPopupTrigger) {
      doPop(e)
    }
    else {
      val x: Int = e.getX
      val y: Int = e.getY
      val row: Int = y / MultiverseViewer.getConfig.paneHeight
      val col: Int = x / MultiverseViewer.getConfig.paneWidth
      if (this.copySource != null) {
        val target: UniverseRunner = this.grid(row)(col).getUniverseRunner
        target.physics.copyData(this.copySource.physics.getData)
        target.randomize(this.rnd)
      }
      else if (this.crossoverParent1 != null && this.crossoverParent2 == null) {
        this.crossoverParent2 = this.grid(row)(col).getUniverseRunner
      }
      else {
        val target: UniverseRunner = this.grid(row)(col).getUniverseRunner
        target.crossover(this.rnd, this.crossoverParent1, this.crossoverParent2)
        target.randomize(this.rnd)
      }
    }
  }

  override def mouseReleased(e: MouseEvent) {
    if (e.isPopupTrigger) {
      doPop(e)
    }
  }

  /**
   * Mutate the selected universe across all free multiverse cells.
   *
   * @param row The row of the universe to generate mutations from.
   * @param col The column of the universe to generate mutations from.
   */
  def mutateAcross(row: Int, col: Int) {
    val sourcePhysics: Physics = this.grid(row)(col).getUniverseRunner.physics
    for(currentRow <- 0 until rows ;
        currentCol <- 0 until cols) {
      if (currentRow != row || currentCol != col) {
        this.grid(currentRow)(currentCol).getUniverseRunner.mutate(this.rnd, sourcePhysics, 0.5, 0.2)
        this.grid(currentRow)(currentCol).getUniverseRunner.randomize(this.rnd)
      }
    }
  }

  /**
   * Mutate a single universe.
   *
   * @param row The multiverse row.
   * @param col The multiverse column.
   */
  def mutateSingle(row: Int, col: Int) {
    val target: UniverseRunner = this.grid(row)(col).getUniverseRunner
    target.mutate(this.rnd, target.physics, 0.5, 0.2)
    target.randomize(this.rnd)
  }

  /**
   * Randomize a universe (both physics and state).
   *
   * @param row The multiverse row.
   * @param col The multiverse column.
   */
  def randomize(row: Int, col: Int) {
    this.grid(row)(col).getUniverseRunner.randomize(this.rnd)
  }

  /**
   * Randomize a universe (not physics, only state).
   *
   * @param row The multiverse row.
   * @param col The multiverse column.
   */
  def reset(row: Int, col: Int) {
    this.grid(row)(col).getUniverseRunner.reset(this.rnd)
  }

  /**
   * Reset all universes (state and physics).
   */
  def resetAll() {
    for(row <- 0 until rows ;
        col <- 0 until cols) {
      reset(row, col)
    }
  }

  /**
   * Run a single universe full screen.
   *
   * @param row The row.
   * @param col The column.
   */
  def runSingular(row: Int, col: Int) {
    this.owner.performStop()
    val sourcePhysics: Physics = this.grid(row)(col).getUniverseRunner.physics
    val v: SingularUniverseViewer = new SingularUniverseViewer(sourcePhysics, 2)
    v.setVisible(true)
  }

  /**
   * Save a universe.
   *
   * @param row The row.
   * @param col The column.
   */
  def save(row: Int, col: Int) {
    try {
      val fc: JFileChooser = new JFileChooser
      fc.setCurrentDirectory(MultiverseViewer.getConfig.saveDirectory)
      val returnVal: Int = fc.showSaveDialog(this)
      if (returnVal == JFileChooser.APPROVE_OPTION) {
        MultiverseViewer.getConfig.saveDirectory = fc.getSelectedFile.getParentFile
        this.grid(row)(col).getUniverseRunner.physics.save(fc.getSelectedFile.toString)
      }
    }
    catch {
      case ex: IOException =>
        ex.printStackTrace()
    }
  }

  /**
   * Set, if we should auto-kill universes that "stabilize" to no movement.
   *
   * @param autoKill True, if autokill is enabled.
   */
  def setAutoKill(autoKill: Boolean) {
    for (element <- this.grid) {
      for (anElement <- element) {
        anElement.getUniverseRunner.autoKill = autoKill
      }
    }
  }

  /**
   * Update the multiverse on screen.
   */
  def update() {
    val tasks: java.util.Collection[AdvanceTask] = new java.util.ArrayList[AdvanceTask]
    for (element <- this.grid) {
      for (anElement <- element) {
        tasks.add(new AdvanceTask(anElement))
      }
    }
    try {
      this.threadPool.invokeAll(tasks)
      val g = getGraphics
      val fm = offscreenGraphics.getFontMetrics
      for(row <- 0 until rows;
          col <- 0 until cols) {
        val x: Int = col * MultiverseViewer.getConfig.paneWidth
        val y: Int = row * MultiverseViewer.getConfig.paneHeight
        val cell: UniversePane = this.grid(row)(col)
        val img: Image = cell.getImage
        this.offscreenGraphics.drawImage(img, x, y, null)
        val selected: UniverseRunner = this.grid(row)(col).getUniverseRunner
        if (this.copySource != null) {
          if (this.copySource eq selected) {
            drawStatus(this.offscreenGraphics, row, col, fm, "Source")
          }
        }
        else if (this.crossoverParent1 != null || this.crossoverParent2 != null) {
          if (selected eq this.crossoverParent1) {
            drawStatus(this.offscreenGraphics, row, col, fm, "Father")
          }
          else if (selected eq this.crossoverParent2) {
            drawStatus(this.offscreenGraphics, row, col, fm, "Mother")
          }
        }
        else {
          val s: String = "diff: " + cell.getUniverseRunner.getDiff + ",age: " + cell.getUniverseRunner.getIterations
          drawStatus(this.offscreenGraphics, row, col, fm, s)
        }
      }
      g.drawImage(this.offscreenImage, 0, 0, this)
    }
    catch {
      case ex: InterruptedException =>
        ex.printStackTrace()
    }
  }

  /**
   * The source for a universe copy.
   */
  private var copySource: UniverseRunner = null
  /**
   * The first parent for a crossover.
   */
  private var crossoverParent1: UniverseRunner = null
  /**
   * The second parent for a crossover.
   */
  private var crossoverParent2: UniverseRunner = null
}