/*
 * Artificial Intelligence for Humans
 * Volume 1: Fundamental Algorithms
 * Scala Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh

 * Copyright 2013 by Jeff Heaton
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
package com.heatonresearch.aifh.examples.randomize

import javax.swing._
import java.awt._
import java.text.DecimalFormat
import java.text.NumberFormat

/**
 * A histogram panel.
 */
class HistoPanel extends JPanel {
  val BAR_WIDTH = 10
  private var evalCount: Long = 0L
  private var evalTime: Long = 0L
  private var evalRate: Int = 0
  private var lastUpdate: Long = 0L
  private var generated: Long = 0L
  private var started: Long = 0L
  private var runtime: Int = 0
  private var low: Double = .0
  private var high: Double = .0
  private val boxes: Array[Int] = Array.ofDim[Int](2000)
  var uniformMode: Boolean = false
  private val formatter = new DecimalFormat("#.##")
  private val intFormatter = NumberFormat.getNumberInstance

  reset()

  def reset() {
    evalCount = 0
    evalTime = 0
    evalRate = 0
    lastUpdate = -1
    generated = 0
    started = -1
    low = Double.PositiveInfinity
    high = Double.NegativeInfinity
    runtime = 0
    for(i <- 0 until boxes.length) {
      boxes(i) = 0
    }
  }

  def reportNumber(d: Double) {
    if (lastUpdate == -1) {
      lastUpdate = System.currentTimeMillis
      started = lastUpdate
    }
    val currentTime: Long = System.currentTimeMillis
    generated += 1
    low = Math.min(low, d)
    high = Math.max(high, d)
    val boxNumber: Int = ((d * 300.0) + 1000.0).toInt
    if (boxNumber >= 0 && boxNumber < boxes.length) {
      boxes(boxNumber) += 1
    }
    evalTime = (currentTime - started) - 5000
    if (evalTime > 0) {
      evalCount += 1
    }
    if ((currentTime - lastUpdate) > 1000) {
      runtime = ((currentTime - started) / 1000).toInt
      lastUpdate = currentTime
      if (evalCount > 0) {
        evalRate = (evalCount / evalTime).toInt
      }
      else {
        evalRate = 0
      }
      repaint()
    }
  }

  override def paint(g: Graphics) {
    val height = getHeight
    val width = getWidth
    g.clearRect(0, 0, width, height)
    val gen: Long = generated / 1024
    val h = g.getFontMetrics.getHeight
    val barCount = width / BAR_WIDTH
    var mode: Int = 0
    for (boxe <- boxes) {
      mode = Math.max(mode, boxe)
    }
    var bar2box: Int = 0
    var boxesIndex: Int = 0
    if (uniformMode) {
      bar2box = 1
      boxesIndex = (boxes.length / 2) + bar2box
    }
    else {
      bar2box = boxes.length / barCount
    }

    for(i <- 0 until barCount) {
      var barAmount: Int = 0
      for(j <- 0 until bar2box) {
        barAmount += boxes(boxesIndex)
        boxesIndex += 1
      }
      barAmount /= bar2box
      val barRatio: Double = barAmount.toDouble / mode
      val barHeight: Int = (barRatio * height).toInt
      g.setColor(Color.CYAN)
      g.fillRect(i * BAR_WIDTH, height - barHeight, BAR_WIDTH, barHeight)
      g.setColor(Color.LIGHT_GRAY)
      g.drawRect(i * BAR_WIDTH, height - barHeight, BAR_WIDTH, barHeight)
    }

    NumberFormat.getNumberInstance.format(35634646)
    var y = h
    g.setColor(Color.BLACK)
    g.drawString("Running for: " + intFormatter.format(runtime) + " seconds", 0, y)
    y += h
    g.drawString("Generated: " + intFormatter.format(gen) + " thousand numbers", 0, y)
    y += h
    g.drawString("Range: " + formatter.format(low) + " to " + formatter.format(high), 0, y)
    y += h
    g.drawString("Rate: " + intFormatter.format(evalRate) + " nums/ms", 0, y)
  }
}
