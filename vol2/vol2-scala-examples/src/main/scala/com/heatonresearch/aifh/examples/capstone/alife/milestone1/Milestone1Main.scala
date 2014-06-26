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

/**
 * This is the first milestone for the plants artificial life capstone.
 * <p/>
 * For this milestone, we would like to just display a "seed".  The seed is the physical starting
 * point for a plant.  Physics imposes limits on seed growth.  The plant genome provides instructions
 * for growth.
 */
object Milestone1Main {
  def main(args: Array[String]) {
    val t: Milestone1Main = new Milestone1Main
    t.setVisible(true)
  }
}

class Milestone1Main extends JFrame {
  import PlantUniverse._
  this.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  private val universe = new PlantUniverse
  universe.reset()
  private val display = new DisplayPlant
  display.universe = this.universe
  this.getContentPane.add(this.display)
  setSize(UNIVERSE_WIDTH * 5, UNIVERSE_HEIGHT * 5)
}