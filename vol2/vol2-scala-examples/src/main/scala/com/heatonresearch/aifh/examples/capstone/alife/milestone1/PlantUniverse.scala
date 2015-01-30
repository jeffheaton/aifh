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

/**
 * This class holds the grid that is the universe that a single plant grows in.  Each plant has its
 * own universe.  Each cell in the grid is either alive or dead.  An alive cell has an energy above
 * the specified threshold.
 * <p/>
 * This class will be used in each of the milestones.  There are several helper functions that provide
 * information about the universe.
 */
object PlantUniverse {
  /**
   * The width of the universe, in terms of cells.
   * Any actual "on screen" display is scaled from this.
   */
  val UNIVERSE_WIDTH: Int = 50
  /**
   * The height of the universe, in terms of cells.
   * Any actual "on screen" display is scaled from this.
   */
  val UNIVERSE_HEIGHT: Int = 100
  /**
   * The location of the ground line.  Anything >= to this is underground.
   */
  val GROUND_LINE: Int = UNIVERSE_HEIGHT - (UNIVERSE_HEIGHT / 3)
  /**
   * The size of a cell "info vector".  This vector identifies a cell's state, and is used to encode instructions
   * in the genome.  All of these are normalized to [0,1].  There are currently four elements:
   * 0: The row that the cell is in.
   * 1: The amount of sunlight the cell is exposed to.
   * 2: The degree of crowding, from neighbors.
   * 3: The amount of nourishment the cell is exposed to.
   */
  val CELL_VECTOR_LENGTH: Int = 4
  /**
   * The size of a GENOME vector.  A genome vector is made up of four "info vectors".  These give instructions
   * on how to grow a plant.
   * Vector 0: Growth template #1
   * Vector 1: Growth template #2
   * Vector 2: Leaf template
   * Vector 3: Stem template
   * For more information on how these are used, refer to the PlantGrowth class.
   */
  val GENOME_SIZE: Int = CELL_VECTOR_LENGTH * 4
  /**
   * The rate that sunlight decays based on shade, or
   * the rate that nourishment decays based on roots absorbing.
   */
  val DECAY: Double = 0.1
  /**
   * The rate at which leafy material turns to wooden stem.
   */
  val STEM_TRANSITION: Double = 0.8
  /**
   * The threshold to allow growth.
   */
  val GROWTH_THRESHOLD: Double = 0.25
  /**
   * The minimum distance to a genome template to execute that instruction.
   * Used to control how growth happens.
   */
  val MIN_GROWTH_DIST: Double = 0.9
  /**
   * The minimum energy that a living cell is allowed to drop to.
   */
  val MIN_LIVING_ENERGY: Double = 0.1
  /**
   * The population size for the genetic algorithm.
   */
  val POPULATION_SIZE: Int = 1000
  /**
   * How many cycles should we allow the plant to grow for?
   */
  val EVALUATION_CYCLES: Int = 100
  /**
   * The required root ratio between how leafy the surface is and how nourished the roots are.
   */
  val REQUIRED_ROOT_RATIO: Double = 0.5
}

class PlantUniverse {
  import PlantUniverse._
  /**
   * The actual grid, that holds the universe.
   */
  private val grid = Array.fill(UNIVERSE_HEIGHT, UNIVERSE_WIDTH)(new PlantUniverseCell)

  /**
   * Get a cell, using row and column index.
   *
   * @param row The row.
   * @param col The column.
   * @return The cell.
   */
  def getCell(row: Int, col: Int): PlantUniverseCell = grid(row)(col)

  /**
   * Calculate the degree of crowding in a cell. Leafy cells
   * produce more crowding than stem.
   *
   * @param row The row.
   * @param col The column.
   * @return The crowd imposed by this cell.
   */
  def calculateCrowd(row: Int, col: Int): Double = {
    if (!isValid(row, col)) return 0
    val cell = getCell(row, col)
    if (!cell.isAlive) {
      return 0
    }
    cell.leafyness
  }

  /**
   * Calculate the degree of crowding around a cell.
   * This is the mean crowding of the
   *
   * @param row The row.
   * @param col The column.
   * @return The mean crowdedness of the cell.
   */
  def calculateMeanNeighborsCrowd(row: Int, col: Int): Double = {
    var sum = 0.0
    sum += calculateCrowd(row - 1, col - 1)
    sum += calculateCrowd(row - 1, col)
    sum += calculateCrowd(row - 1, col + 1)
    sum += calculateCrowd(row, col - 1)
    sum += calculateCrowd(row, col + 1)
    sum += calculateCrowd(row + 1, col - 1)
    sum += calculateCrowd(row + 1, col)
    sum += calculateCrowd(row + 1, col + 1)
    sum / 8.0
  }

  /**
   * Return an info vector about a cell.  This allows cells to be identified by instructions in the genome.
   * The vector contains four doubles.  All doubles range from [0,1].
   * <p/>
   * Element 0: The height of the cell. 1.0 for the last row and 0.0 for the first row.
   * Element 1: The amount of sunlight (for surface cells) or water (for underground cells) exposure for this cell.
   * Element 2: Crowding by neighbors.
   * Element 3: Nourishment for this cell.
   *
   * @param row The row.
   * @param col The column.
   * @return The info vector.
   */
  def getCellInfoVector(row: Int, col: Int): Array[Double] = {
    val result = new Array[Double](CELL_VECTOR_LENGTH)
    val cell = getCell(row, col)
    result(0) = row / PlantUniverse.UNIVERSE_HEIGHT
    if (row < PlantUniverse.GROUND_LINE) {
      result(1) = cell.calculatedSunlight
    }
    else {
      result(1) = cell.calculatedWater
    }
    result(2) = calculateMeanNeighborsCrowd(row, col)
    result(3) = cell.nourishment
    result
  }

  /**
   * Reset the entire grid to a single seed.
   */
  def reset() {
    for (aGrid <- grid) {
      for (cell <- aGrid) {
        cell.leafyness = 0
        cell.energy = 0
        cell.nourishment = 0
      }
    }
    val center: Int = PlantUniverse.UNIVERSE_WIDTH / 2
    val groundLevel: Int = PlantUniverse.GROUND_LINE
    grid(groundLevel)(center).leafyness = 0
    grid(groundLevel)(center).nourishment = 1
    grid(groundLevel)(center).energy = 1
    grid(groundLevel - 1)(center).leafyness = 0.5
    grid(groundLevel - 1)(center).nourishment = 1
    grid(groundLevel - 1)(center).energy = 1
    grid(groundLevel - 2)(center).leafyness = 1
    grid(groundLevel - 2)(center).nourishment = 1
    grid(groundLevel - 2)(center).energy = 1
  }

  /**
   * Returns true if a cell is valid. Invalid cells are off the bounds of a grid.
   *
   * @param row The row.
   * @param col The column.
   * @return True, if valid.
   */
  def isValid(row: Int, col: Int): Boolean = {
    if (row < 0 || col < 0) {
      return false
    }
    if (row >= this.grid.length) {
      return false
    }
    if (col >= this.grid(row).length) {
      return false
    }
    true
  }

  /**
   * Calculate the energy for a cell.
   *
   * @param row The row.
   * @param col The column.
   * @return The info vector.
   */
  def calculateEnergy(row: Int, col: Int): Double = {
    if (!isValid(row, col)) {
      return 0
    }
    this.grid(row)(col).energy
  }

  /**
   * Calculate the transfer energy for a cell.  This is the amount of energy transferred into a cell.
   *
   * @param row The row.
   * @param col The column.
   * @return The amount of energy transferred in.
   */
  def calculateTransferEnergy(row: Int, col: Int): Double = {
    var result: Double = 0
    result = Math.max(result, calculateEnergy(row - 1, col - 1))
    result = Math.max(result, calculateEnergy(row - 1, col))
    result = Math.max(result, calculateEnergy(row - 1, col + 1))
    result
  }

  /**
   * Calculate the transfer nourishment for a cell.  This is the amount of nourishment transferred into a cell.
   *
   * @param row The row.
   * @param col The column.
   * @return The amount of energy transferred in.
   */
  def calculateTransferNourishment(row: Int, col: Int): Double = {
    var result: Double = 0
    result = Math.max(result, calculateEnergy(row + 1, col - 1))
    result = Math.max(result, calculateEnergy(row + 1, col))
    result = Math.max(result, calculateEnergy(row + 1, col + 1))
    result
  }

  /**
   * Count the number of live cells as neighbors to a cell.
   *
   * @param row The row.
   * @param col The column.
   * @return The neighbor count.
   */
  def countNeighbors(row: Int, col: Int): Int = {
    var sum: Int = 0
    if (isAlive(row - 1, col)) {
      sum += 1
    }
    if (isAlive(row + 1, col)) {
      sum += 1
    }
    if (isAlive(row, col - 1)) {
      sum += 1
    }
    if (isAlive(row, col + 1)) {
      sum += 1
    }
    if (isAlive(row - 1, col - 1)) {
      sum += 1
    }
    if (isAlive(row + 1, col + 1)) {
      sum += 1
    }
    if (isAlive(row - 1, col + 1)) {
      sum += 1
    }
    if (isAlive(row + 1, col - 1)) {
      sum += 1
    }
    sum
  }

  /**
   * Returns true, if the specified cell can grow.
   *
   * @param row The row.
   * @param col The column.
   * @return True, if the specified cell is allowed to grow.
   */
  def canGrow(row: Int, col: Int): Boolean = {
    val cell: PlantUniverseCell = getCell(row, col)
    if (cell.isAlive) {
      if (row >= PlantUniverse.GROUND_LINE) {
        return countNeighbors(row, col) < 4
      }
      else {
        return cell.energy > PlantUniverse.GROWTH_THRESHOLD && cell.nourishment > PlantUniverse.GROWTH_THRESHOLD
      }
    }
    false
  }

  /**
   * Returns true, if the specified cell is alive. Alive cells have energy.
   *
   * @param row The row.
   * @param col The column.
   * @return True, if the specified cell is alive to grow.
   */
  def isAlive(row: Int, col: Int): Boolean = isValid(row, col) && grid(row)(col).isAlive

  /**
   * The amount of nourishment that is held inside of the roots.  This is used to calculate the
   * root ratio, that limits growth.
   */
  var rootCount: Double = 0.0
  /**
   * The amount of leafy material above the surface.  This is used to calculate the root ratio,
   * that limits growth.
   */
  var surfaceCount: Double = 0.0
}