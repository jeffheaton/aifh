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
package com.heatonresearch.aifh.examples.capstone.alife.milestone3

import com.heatonresearch.aifh.examples.capstone.alife.milestone1.PlantUniverse
import com.heatonresearch.aifh.examples.capstone.alife.milestone2.PlantGrowth
import com.heatonresearch.aifh.examples.capstone.alife.milestone2.PlantPhysics
import com.heatonresearch.aifh.genetic.genome.DoubleArrayGenome
import com.heatonresearch.aifh.learning.MLMethod
import com.heatonresearch.aifh.learning.score.ScoreFunction

/**
 * This class is used to score the plant.  Plants are scored for how green they are after a specified
 * number of iterations.
 */
class PlantScore extends ScoreFunction {

  import PlantUniverse._
  override def calculateScore(algo: MLMethod): Double = {
    val genome = algo.asInstanceOf[DoubleArrayGenome]
    val universe = new PlantUniverse
    universe.reset()
    val physics = new PlantPhysics
    val growth = new PlantGrowth
    for(i <- 0 until EVALUATION_CYCLES) {
      physics.runPhysics(universe)
      growth.runGrowth(universe, genome.getData)
    }

    var count = 0
    var sum = 0.0
    for(row <- 0 until UNIVERSE_HEIGHT ;
        col <- 0 until UNIVERSE_WIDTH) {
      val cell = universe.getCell(row, col)
      if (cell.isAlive) {
        if (row >= GROUND_LINE) {
          sum += 0.5
        }
        else {
          sum += cell.leafyness
        }
      }
      count += 1
    }
    sum / count
  }

  override def shouldMinimize: Boolean = false
}