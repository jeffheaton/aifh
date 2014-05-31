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
package com.heatonresearch.aifh.examples.capstone.alife.milestone1

import javax.swing._
import java.awt._

/**
 * A panel used to display a plant. This panel will be used in all milestones.
 */
object DisplayPlant {
  /**
   * The most green color that a plant can take on.
   */
  private val FULL_GREEN: Color = new Color(0, 255, 0)
  /**
   * The most brown color that a plant can take on.
   */
  private val FULL_BROWN: Color = new Color(165, 42, 42)
  /**
   * The color of the sky.
   */
  private val SKY_COLOR: Color = new Color(135, 206, 250)
  /**
   * The color of the dirt.
   */
  private val DIRT_COLOR: Color = new Color(96, 96, 96)
}

class DisplayPlant extends JPanel {
  import DisplayPlant._
  import PlantUniverse._


  /**
   * The universe that we will render from.
   */
  var universe: PlantUniverse = null
  /**
   * A gradient of colors between full green and brown.
   */
  private var gradient: Array[Color] = null
  /**
   * The best score, to display.
   */
  var bestScore: Double = .0
  /**
   * The current generation, for display.
   */
  var generation: Int = 0

  {
    val gradentRangeRed: Int = FULL_GREEN.getRed - FULL_BROWN.getRed
    val gradentRangeGreen: Int = FULL_GREEN.getGreen - FULL_BROWN.getGreen
    val gradentRangeBlue: Int = FULL_GREEN.getBlue - FULL_BROWN.getBlue
    val maxRange: Int = Math.max(Math.max(Math.abs(gradentRangeRed), Math.abs(gradentRangeGreen)), Math.abs(gradentRangeBlue))
    val scaleRed: Double = gradentRangeRed.asInstanceOf[Double] / maxRange.asInstanceOf[Double]
    val scaleGreen: Double = gradentRangeGreen.asInstanceOf[Double] / maxRange.asInstanceOf[Double]
    val scaleBlue: Double = gradentRangeBlue.asInstanceOf[Double] / maxRange.asInstanceOf[Double]
    this.gradient = Array.ofDim[Color](maxRange)
    for(i <- 0 until maxRange) {
      gradient(i) = new Color((FULL_BROWN.getRed + (i * scaleRed)).asInstanceOf[Int], (FULL_BROWN.getGreen + (i * scaleGreen)).asInstanceOf[Int], (FULL_BROWN.getBlue + (i * scaleBlue)).asInstanceOf[Int])
    }

  }

  /**
   * Paint the plant.
   *
   * @param g The graphics object.
   */
  override def paint(g: Graphics) {
    val width = getWidth
    val height = getHeight
    val cellWidth = Math.max(width / UNIVERSE_WIDTH, 1)
    val cellHeight = Math.max(height / UNIVERSE_HEIGHT, 1)
    for(row <- 0 until UNIVERSE_HEIGHT;
        col <- 0 until UNIVERSE_WIDTH) {
      val cell = universe.getCell(row, col)
      if (row >= GROUND_LINE) {
        if (cell.isAlive) {
          g.setColor(Color.WHITE)
        }
        else {
          g.setColor(DIRT_COLOR)
        }
      }
      else {
        if (cell.isAlive) {
          println(this.gradient)
          val idx = ((this.gradient.length - 1) * cell.leafyness).asInstanceOf[Int]
          g.setColor(this.gradient(idx))
        }
        else {
          g.setColor(SKY_COLOR)
        }
      }
      g.fillRect(col * cellWidth, row * cellHeight, cellWidth, cellHeight)
    }
    if (generation > 0) {
      var y: Int = cellHeight * UNIVERSE_HEIGHT
      g.setColor(Color.white)
      g.fillRect(0, y, width, height)
      val fm = g.getFontMetrics
      y += fm.getHeight
      g.setColor(Color.black)
      g.drawString("Generation: " + this.generation, 0, y)
      y += fm.getHeight
      g.drawString("Best Score:  " + this.bestScore, 0, y)
    }
  }
}