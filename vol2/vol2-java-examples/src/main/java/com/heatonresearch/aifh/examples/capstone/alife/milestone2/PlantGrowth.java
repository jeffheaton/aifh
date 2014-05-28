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
package com.heatonresearch.aifh.examples.capstone.alife.milestone2;

import com.heatonresearch.aifh.distance.EuclideanDistance;
import com.heatonresearch.aifh.examples.capstone.alife.milestone1.PlantUniverse;
import com.heatonresearch.aifh.examples.capstone.alife.milestone1.PlantUniverseCell;

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
public class PlantGrowth {
    /**
     * Transformations to move from a cell to the 9 neighboring cells.
     * These are the column values.
     */
    private final int[] colTransform = {0, 0, -1, 1, -1, 1, 1, -1};

    /**
     * Transformations to move from a cell to the 9 neighboring cells.
     * These are the row values.
     */
    private final int[] rowTransform = {-1, 1, 0, 0, -1, 1, -1, 1};

    /**
     * Euclidean distance is used to calculate distances to the genome vectors.
     */
    private final EuclideanDistance dist = new EuclideanDistance();

    /**
     * Used to hold the new cells that have grown.
     */
    private final boolean[][] newComposition = new boolean[PlantUniverse.UNIVERSE_HEIGHT][PlantUniverse.UNIVERSE_WIDTH];

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
    private double getGrowthPotential(PlantUniverse universe, int row, int col, double[] genome) {
        double[] cellVec = universe.getCellInfoVector(row, col);
        double d1 = dist.calculate(cellVec, 0, genome, PlantUniverse.CELL_VECTOR_LENGTH * 2, PlantUniverse.CELL_VECTOR_LENGTH);
        double d2 = dist.calculate(cellVec, 0, genome, PlantUniverse.CELL_VECTOR_LENGTH * 3, PlantUniverse.CELL_VECTOR_LENGTH);

        double result = Math.min(d1, d2);
        if (result > PlantUniverse.MIN_GROWTH_DIST) {
            result = -1;
        }

        return result;

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
    private void evaluateNeighbors(PlantUniverse universe, int row, int col, double[] genome, boolean allowRoot, boolean allowSurface) {
        int growthTargetRow = row;
        int growthTargetCol = col;
        double growthTargetScore = Double.POSITIVE_INFINITY;

        for (int i = 0; i < colTransform.length; i++) {
            int evalCol = col + colTransform[i];
            int evalRow = row + rowTransform[i];

            if (!allowRoot && evalRow >= PlantUniverse.GROUND_LINE) {
                continue;
            }

            if (!allowSurface && evalRow < PlantUniverse.GROUND_LINE) {
                continue;
            }

            if (universe.isValid(evalRow, evalCol)) {
                double p = getGrowthPotential(universe, evalRow, evalCol, genome);
                if (p > 0) {
                    if (p < growthTargetScore) {
                        growthTargetScore = p;
                        growthTargetRow = evalRow;
                        growthTargetCol = evalCol;
                    }
                }
            }
        }

        // Grow new cell, if requested, did we ever set target row & col to anything?
        if (growthTargetRow != row || growthTargetCol != col) {
            this.newComposition[growthTargetRow][growthTargetCol] = true;
        }
    }

    /**
     * Run a growth cycle for the universe.
     *
     * @param universe The universe.
     * @param genome   The genome.
     */
    public void runGrowth(PlantUniverse universe, double[] genome) {
        // Does this plant have enough roots to grow?
        if (universe.getSurfaceCount() == 0) {
            return;
        }

        // The amount of leafy material per root nourishment.  A higher number indicates
        // more root nourishment than leafs.
        double rootRatio = universe.getRootCount() / universe.getSurfaceCount();

        boolean allowRoot = rootRatio < 0.5; //rootRatio < 0.1;
        boolean allowSurface = rootRatio > 0.5;

        // Reset the new composition to be the composition of the current universe
        for (int row = 0; row < PlantUniverse.UNIVERSE_HEIGHT; row++) {
            for (int col = 0; col < PlantUniverse.UNIVERSE_WIDTH; col++) {
                this.newComposition[row][col] = false;
            }
        }

        for (int row = 0; row < PlantUniverse.UNIVERSE_HEIGHT; row++) {
            for (int col = 0; col < PlantUniverse.UNIVERSE_WIDTH; col++) {
                PlantUniverseCell cell = universe.getCell(row, col);

                // see if we want to change the composition
                if (row < PlantUniverse.GROUND_LINE) {
                    double[] cellVec = universe.getCellInfoVector(row, col);
                    double d1 = dist.calculate(cellVec, 0, genome, 0, PlantUniverse.CELL_VECTOR_LENGTH);
                    double d2 = dist.calculate(cellVec, 0, genome, PlantUniverse.CELL_VECTOR_LENGTH, PlantUniverse.CELL_VECTOR_LENGTH);

                    if (d1 < d2) {
                        cell.setLeafyness(cell.getLeafyness() * PlantUniverse.STEM_TRANSITION);
                    }
                }

                // Evaluate growth into each neighbor cell
                if (universe.canGrow(row, col)) {
                    evaluateNeighbors(universe, row, col, genome, allowRoot, allowSurface);
                }
            }
        }

        // Copy the new composition back to the universe
        for (int row = 0; row < PlantUniverse.UNIVERSE_HEIGHT; row++) {
            for (int col = 0; col < PlantUniverse.UNIVERSE_WIDTH; col++) {
                PlantUniverseCell cell = universe.getCell(row, col);

                if (this.newComposition[row][col]) {
                    if (row >= PlantUniverse.GROUND_LINE) {
                        // Roots are always 100% stem for transfer.
                        cell.setLeafyness(0);
                    } else {
                        cell.setLeafyness(1.0);
                    }
                    cell.setEnergy(1.0);
                    cell.setNourishment(1.0);
                }
            }
        }
    }
}
