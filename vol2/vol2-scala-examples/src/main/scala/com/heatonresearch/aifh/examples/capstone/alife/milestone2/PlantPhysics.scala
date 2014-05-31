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
package com.heatonresearch.aifh.examples.capstone.alife.milestone2

import com.heatonresearch.aifh.examples.capstone.alife.milestone1.PlantUniverse

/**
 * The physics class defines limits on the growth that the genome wants to implement.  Specifically, the physics
 * determines how sunlight is absorbed and nourishment is distributed in the plant.
 * <p/>
 * Sunlight comes from above and stops at the ground level. More leafy material absorbs sunlight and reduces it
 * because of shade.  Water comes from below and is absorbed by the roots.
 */
class PlantPhysics {
  import PlantUniverse._
  /**
   * Distribute the sunlight energy in the universe.
   *
   * @param universe The universe.
   */
  private def distributeEnergy(universe: PlantUniverse) {
    val sunlight = Array.fill[Double](UNIVERSE_WIDTH)(1.0)
    for(row <- 0 until UNIVERSE_HEIGHT ;
        col <- 0 until UNIVERSE_WIDTH) {
      var decay = 0.0
      if (row >= GROUND_LINE) {
        decay = 0
      }
      else {
        decay = 1
      }
      val cell = universe.getCell(row, col)
      cell.calculatedSunlight = sunlight(col)
      if (cell.isAlive) {
        decay *= DECAY * cell.leafyness
        val myEnergy = cell.calculatedSunlight * cell.leafyness
        val transEnergy = universe.calculateTransferEnergy(row, col) * (1.0 - cell.leafyness)
        var e = Math.max(myEnergy, transEnergy)
        e = Math.max(MIN_LIVING_ENERGY, e)
        cell.energy = e
      }
      sunlight(col) *= decay
    }
  }

  /**
   * Distribute nourishment in the universe.
   *
   * @param universe The universe.
   */
  private def distributeNourishment(universe: PlantUniverse) {
    var rootCount: Double = 0.0
    var surfaceCount: Double = 0
    val waterTable = Array.fill[Double](UNIVERSE_WIDTH)(1.0)
    for(row <- 0 until UNIVERSE_HEIGHT ;
        col <- 0 until UNIVERSE_WIDTH) {
      var decay: Double = .0
      if (row < GROUND_LINE) {
        decay = 0
      }
      else {
        decay = 1
      }
      val cell = universe.getCell(row, col)
      cell.calculatedWater = waterTable(col)
      if (cell.isAlive) {
        decay *= DECAY
        val myWater = cell.calculatedWater * cell.leafyness
        val transWater = universe.calculateTransferNourishment(row, col) * (1.0 - cell.leafyness)
        var n = Math.max(myWater, transWater)
        n = Math.max(MIN_LIVING_ENERGY, n)
        cell.nourishment = n
        if (row >= GROUND_LINE) {
          rootCount += cell.nourishment
        }
        else {
          surfaceCount += cell.leafyness
        }
      }
      waterTable(col) *= decay
    }
    universe.rootCount = rootCount
    universe.surfaceCount = surfaceCount
  }

  def runPhysics(universe: PlantUniverse) {
    distributeEnergy(universe)
    distributeNourishment(universe)
  }
}