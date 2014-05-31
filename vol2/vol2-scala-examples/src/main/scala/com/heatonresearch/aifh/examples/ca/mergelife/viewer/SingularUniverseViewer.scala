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

import com.heatonresearch.aifh.examples.ca.mergelife.physics.MergePhysics
import com.heatonresearch.aifh.examples.ca.mergelife.physics.Physics
import com.heatonresearch.aifh.examples.ca.mergelife.universe.Universe
import com.heatonresearch.aifh.examples.ca.mergelife.universe.UniverseRunner
import com.heatonresearch.aifh.examples.ca.mergelife.universe.UniverseVisualizer
import com.heatonresearch.aifh.randomize.GenerateRandom
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom
import javax.swing._
import java.awt._
import java.awt.event.ComponentEvent
import java.awt.event.ComponentListener
import java.awt.event.WindowEvent
import java.awt.event.WindowListener

/**
 * View a single universe.
 */
class SingularUniverseViewer(thePhysics: Physics,val zoom: Int) extends JFrame with ComponentListener with Runnable with WindowListener {
  /**
   * The universe runner.
   */
  private var runner: UniverseRunner = null
  /**
   * Is the universe running?
   */
  private var running: Boolean = false
  /**
   * The source data for the universe physics.
   */
  private val sourceData: Array[Double] = thePhysics.getData.clone()
  /**
   * Stop request.
   */
  private var stopRunning: Boolean = false
  /**
   * The visualizer.
   */
  private var visual: UniverseVisualizer = null
  /**
   * The random number generator.
   */
  private val rnd: GenerateRandom = new MersenneTwisterGenerateRandom

  setTitle("Multiverse")
  setSize(1024, 768)
  addComponentListener(this)
  addWindowListener(this)

  //----- end of constructor ----

  override def componentHidden(e: ComponentEvent) {}

  override def componentMoved(e: ComponentEvent) {}

  override def componentResized(e: ComponentEvent) {
    val width: Int = getWidth / this.zoom
    val height: Int = getHeight / this.zoom
    val universe: Universe = new Universe(height, width, 3)
    val physics: Physics = new MergePhysics(universe)
    physics.copyData(this.sourceData)
    universe.randomize(this.rnd)
    this.runner = new UniverseRunner(universe, physics)
    this.visual = new UniverseVisualizer(universe, this.zoom)
    if (!this.running) {
      this.running = true
      val t: Thread = new Thread(this)
      t.start()
    }
  }

  override def componentShown(e: ComponentEvent) {}

  override def run() {
    val g: Graphics = getGraphics
    this.stopRunning = false
    while (!this.stopRunning) {
      this.runner.advance(this.rnd)
      val image: Image = this.visual.visualize
      g.drawImage(image, 0, 0, null)
    }
  }

  override def windowActivated(e: WindowEvent) {}

  override def windowClosed(e: WindowEvent) {
    this.stopRunning = true
  }

  override def windowClosing(e: WindowEvent) {}

  override def windowDeactivated(e: WindowEvent) {}

  override def windowDeiconified(e: WindowEvent) {}

  override def windowIconified(e: WindowEvent) {}

  override def windowOpened(e: WindowEvent) {}
}