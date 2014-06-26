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

import com.heatonresearch.aifh.AIFH

/**
 * An individual cell in the plant universe.
 */
class PlantUniverseCell {
  /**
   * @return True, if this cell is alive.
   */
  def isAlive: Boolean = energy > AIFH.DEFAULT_PRECISION

  /**
   * How green (leaf) or brown (trunk) is the cell.  1.0 is fully leaf, 0.0 is fully trunk.
   */
  var leafyness: Double = 0
  /**
   * The amount of energy between [0,1].
   */
  var energy: Double = 0
  /**
   * The amount of nourishment between [0,1].
   */
  var nourishment: Double = 0
  /**
   * The calculated sunlight exposure.
   */
  var calculatedSunlight: Double = 0
  /**
   * The calculated water exposure.
   */
  var calculatedWater: Double = 0
}