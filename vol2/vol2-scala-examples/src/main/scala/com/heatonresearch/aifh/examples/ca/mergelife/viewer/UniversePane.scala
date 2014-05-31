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
import java.awt._

/**
* Universe display pane.
*/
class UniversePane {

  /** The universe rendered. */
  private var image: Image = null
  /** A random number generator. */
  private val rnd: GenerateRandom = new MersenneTwisterGenerateRandom

  val width = MultiverseViewer.getConfig.paneWidth / MultiverseViewer.getConfig.getZoom
  val height = MultiverseViewer.getConfig.paneHeight / MultiverseViewer.getConfig.getZoom
  val universe = new Universe(height, width, 3)
  val physics: Physics = new MergePhysics(universe)
  universe.randomize(rnd)
  physics.randomize()
  /** The universe runner. */
  private val universeRunner = new UniverseRunner(universe, physics)
  /** The universe visualizer. */
  private val visualizer = new UniverseVisualizer(universe, MultiverseViewer.getConfig.getZoom)


  /**
   * Advance a frame.
   */
  def advance() {
    this.universeRunner.advance(this.rnd)
  }

  /**
   * @return The universe image.
   */
  def getImage: Image = image

  /**
   * @return The universe runner.
   */
  def getUniverseRunner: UniverseRunner = universeRunner

  /**
   * Visuzlizr the universe.
   */
  def visualize() {
    this.image = this.visualizer.visualize
  }

}