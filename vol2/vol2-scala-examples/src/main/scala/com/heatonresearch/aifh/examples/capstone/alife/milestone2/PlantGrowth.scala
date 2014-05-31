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

import com.heatonresearch.aifh.distance.EuclideanDistance
import com.heatonresearch.aifh.examples.capstone.alife.milestone1.PlantUniverse
import com.heatonresearch.aifh.examples.capstone.alife.milestone1.PlantUniverseCell

/**
 * This class provides a growth cycle for the plant genome.  This class runs the "program" embedded in the plant's
 * genome.  A plant genome is an array of four vectors.  Each vector is of length 4, so the entire genome is
 * 4*4 = 16 double values.
 * <p/>
 * Each of the four vectors corresponds to a cell's info vector.  A cell info vector provides information about the
 * state of a cell.  By cell, I mean a grid cell.  The grid cell can be either living (filled) or dead (empty).
 * The info vector for a cell is calculated as follows.
 * <p/>
 * Element 0: The height of the cell. 1.0 for the last row and 0.0 for the first row.
 * Element 1: The amount of sunlight (for surface cells) or water (for underground cells) exposure for this cell.
 * Element 2: Crowding by neighbors.
 * Element 3: Nourishment for this cell.
 * <p/>
 * The genome's vectors are as follows:
 * <p/>
 * Vector 0: Stem desired
 * Vector 1: Leaf desired
 * Vector 2: Growth Option #1
 * Vector 3: Growth Option #2
 * <p/>
 * Vectors 0 and 1 go together.  For each living cell we see if its info vector is closer to vector 0 or vector 1.
 * If it is closer to stem (0), then the leafyness attribute of the cell is decreased.  Leaf's can only move towards
 * stem.  A stem cannot change back into a leaf.
 * <p/>
 * Vectors 2 and 3 also go together. When a plant cell is eligible for growth, it evaluates all neighbor cells to
 * see which it wants to grow into.  What ever neighbor cell is closest to ether vector 2 or 3 is chosen.  If
 * the candidate cell is not lower than a specific threshold to either, then no growth occurs.
 * <p/>
 * A ratio between how leafy the surface is, and roots must also be maintained for growth to occur.
 */
object PlantGrowth {
  /**
   * Transformations to move from a cell to the 9 neighboring cells.
   * These are the column values.
   */
  private val colTransform: Array[Int] = Array(0, 0, -1, 1, -1, 1, 1, -1)
  /**
   * Transformations to move from a cell to the 9 neighboring cells.
   * These are the row values.
   */
  private val rowTransform: Array[Int] = Array(-1, 1, 0, 0, -1, 1, -1, 1)
}

class PlantGrowth {
  import PlantUniverse._
  /**
   * Euclidean distance is used to calculate distances to the genome vectors.
   */
  private val dist = new EuclideanDistance
  /**
   * Used to hold the new cells that have grown.
   */
  private val newComposition = Array.ofDim[Boolean](UNIVERSE_HEIGHT, UNIVERSE_WIDTH)

  import PlantGrowth._
  /**
   * Calculate the growth potential for a candidate cell. Evaluates the distance between the candidate cell's info
   * vector and the two growth vectors in the genome.  The minimum of these two vectors will be returned if
   * it is below a specified minimum threshold.
   *
   * @param universe The universe to evaluate.
   * @param row      The row to evaluate.
   * @param col      The column to evaluate.
   * @param genome   The genome.
   * @return The minimum distance.
   */
  private def getGrowthPotential(universe: PlantUniverse, row: Int, col: Int, genome: Array[Double]): Double = {
    val cellVec = universe.getCellInfoVector(row, col)
    val d1 = dist.calculate(cellVec, 0, genome, CELL_VECTOR_LENGTH * 2, CELL_VECTOR_LENGTH)
    val d2 = dist.calculate(cellVec, 0, genome, CELL_VECTOR_LENGTH * 3, CELL_VECTOR_LENGTH)
    var result = Math.min(d1, d2)
    if (result > MIN_GROWTH_DIST) {
      result = -1
    }
    result
  }

  /**
   * Evaluate neighbors to see where to grow into.
   *
   * @param universe     The universe.
   * @param row          The row.
   * @param col          The column.
   * @param genome       The genome.
   * @param allowRoot    Are roots allowed?
   * @param allowSurface Is surface growth allowed.
   */
  private def evaluateNeighbors(universe: PlantUniverse, row: Int, col: Int, genome: Array[Double], allowRoot: Boolean,
                                allowSurface: Boolean) {
    var growthTargetRow: Int = row
    var growthTargetCol: Int = col
    var growthTargetScore: Double = Double.PositiveInfinity

    class ContinueException extends Exception

    try {
      for(i <- 0 until colTransform.length) {
        val evalCol: Int = col + colTransform(i)
        val evalRow: Int = row + rowTransform(i)
        if (!allowRoot && evalRow >= GROUND_LINE) {
          throw new ContinueException
        }
        if (!allowSurface && evalRow < GROUND_LINE) {
          throw new ContinueException
        }
        if (universe.isValid(evalRow, evalCol)) {
          val p: Double = getGrowthPotential(universe, evalRow, evalCol, genome)
          if (p > 0) {
            if (p < growthTargetScore) {
              growthTargetScore = p
              growthTargetRow = evalRow
              growthTargetCol = evalCol
            }
          }
        }
      }
    } catch {
      case _ : ContinueException =>
    }


    if (growthTargetRow != row || growthTargetCol != col) {
      this.newComposition(growthTargetRow)(growthTargetCol) = true
    }
  }

  /**
   * Run a growth cycle for the universe.
   *
   * @param universe The universe.
   * @param genome   The genome.
   */
  def runGrowth(universe: PlantUniverse, genome: Array[Double]) {
    if (universe.surfaceCount == 0) {
      return
    }
    val rootRatio = universe.rootCount / universe.surfaceCount
    val allowRoot = rootRatio < 0.5
    val allowSurface = rootRatio > 0.5
    for(row <- 0 until UNIVERSE_HEIGHT ;
        col <- 0 until UNIVERSE_WIDTH) {
      this.newComposition(row)(col) = false
    }

    for(row <- 0 until UNIVERSE_HEIGHT ;
        col <- 0 until UNIVERSE_WIDTH) {
      val cell = universe.getCell(row, col)
      if (row < GROUND_LINE) {
        val cellVec: Array[Double] = universe.getCellInfoVector(row, col)
        val d1: Double = dist.calculate(cellVec, 0, genome, 0, CELL_VECTOR_LENGTH)
        val d2: Double = dist.calculate(cellVec, 0, genome, CELL_VECTOR_LENGTH, CELL_VECTOR_LENGTH)
        if (d1 < d2) {
          cell.leafyness = cell.leafyness * STEM_TRANSITION
        }
      }
      if (universe.canGrow(row, col)) {
        evaluateNeighbors(universe, row, col, genome, allowRoot, allowSurface)
      }
    }

    for(row <- 0 until UNIVERSE_HEIGHT ;
        col <- 0 until UNIVERSE_WIDTH) {
      val cell: PlantUniverseCell = universe.getCell(row, col)
      if (this.newComposition(row)(col)) {
        if (row >= GROUND_LINE) {
          cell.leafyness = 0
        }
        else {
          cell.leafyness = 1.0
        }
        cell.energy = 1.0
        cell.nourishment = 1.0
      }
    }
  }

}