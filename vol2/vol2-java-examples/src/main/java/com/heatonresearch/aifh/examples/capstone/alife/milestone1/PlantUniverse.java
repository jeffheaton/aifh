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
package com.heatonresearch.aifh.examples.capstone.alife.milestone1;

/**
 * This class holds the grid that is the universe that a single plant grows in.  Each plant has its
 * own universe.  Each cell in the grid is either alive or dead.  An alive cell has an energy above
 * the specified threshold.
 * <p/>
 * This class will be used in each of the milestones.  There are several helper functions that provide
 * information about the universe.
 */
public class PlantUniverse {
    /**
     * The width of the universe, in terms of cells.
     * Any actual "on screen" display is scaled from this.
     */
    public static final int UNIVERSE_WIDTH = 50;

    /**
     * The height of the universe, in terms of cells.
     * Any actual "on screen" display is scaled from this.
     */
    public static final int UNIVERSE_HEIGHT = 100;

    /**
     * The location of the ground line.  Anything >= to this is underground.
     */
    public static final int GROUND_LINE = UNIVERSE_HEIGHT - (UNIVERSE_HEIGHT / 3);

    /**
     * The size of a cell "info vector".  This vector identifies a cell's state, and is used to encode instructions
     * in the genome.  All of these are normalized to [0,1].  There are currently four elements:
     * 0: The row that the cell is in.
     * 1: The amount of sunlight the cell is exposed to.
     * 2: The degree of crowding, from neighbors.
     * 3: The amount of nourishment the cell is exposed to.
     */
    public static final int CELL_VECTOR_LENGTH = 4;

    /**
     * The size of a GENOME vector.  A genome vector is made up of four "info vectors".  These give instructions
     * on how to grow a plant.
     * Vector 0: Growth template #1
     * Vector 1: Growth template #2
     * Vector 2: Leaf template
     * Vector 3: Stem template
     * For more information on how these are used, refer to the PlantGrowth class.
     */
    public static final int GENOME_SIZE = CELL_VECTOR_LENGTH * 4;

    /**
     * The rate that sunlight decays based on shade, or
     * the rate that nourishment decays based on roots absorbing.
     */
    public static final double DECAY = 0.1;

    /**
     * The rate at which leafy material turns to wooden stem.
     */
    public static final double STEM_TRANSITION = 0.8;

    /**
     * The threshold to allow growth.
     */
    public static final double GROWTH_THRESHOLD = 0.25;


    /**
     * The minimum distance to a genome template to execute that instruction.
     * Used to control how growth happens.
     */
    public static final double MIN_GROWTH_DIST = 0.9;

    /**
     * The minimum energy that a living cell is allowed to drop to.
     */
    public static final double MIN_LIVING_ENERGY = 0.1;

    /**
     * The population size for the genetic algorithm.
     */
    public static final int POPULATION_SIZE = 1000;

    /**
     * How many cycles should we allow the plant to grow for?
     */
    public static final int EVALUATION_CYCLES = 100;

    /**
     * The required root ratio between how leafy the surface is and how nourished the roots are.
     */
    public static final double REQUIRED_ROOT_RATIO = 0.5;

    /**
     * The actual grid, that holds the universe.
     */
    private PlantUniverseCell[][] grid = new PlantUniverseCell[UNIVERSE_HEIGHT][UNIVERSE_WIDTH];

    /**
     * The amount of nourishment that is held inside of the roots.  This is used to calculate the
     * root ratio, that limits growth.
     */
    private double rootCount;

    /**
     * The amount of leafy material above the surface.  This is used to calculate the root ratio,
     * that limits growth.
     */
    private double surfaceCount;

    /**
     * Construct the universe and create the grid.
     */
    public PlantUniverse() {
        for (int row = 0; row < grid.length; row++) {
            for (int col = 0; col < grid[row].length; col++) {
                this.grid[row][col] = new PlantUniverseCell();
            }
        }
    }

    /**
     * Get a cell, using row and column index.
     *
     * @param row The row.
     * @param col The column.
     * @return The cell.
     */
    public PlantUniverseCell getCell(int row, int col) {
        return this.grid[row][col];
    }

    /**
     * Calculate the degree of crowding in a cell. Leafy cells
     * produce more crowding than stem.
     *
     * @param row The row.
     * @param col The column.
     * @return The crowd imposed by this cell.
     */
    public double calculateCrowd(int row, int col) {
        if (!isValid(row, col))
            return 0;

        PlantUniverseCell cell = getCell(row, col);

        if (!cell.isAlive()) {
            return 0;
        }

        return cell.getLeafyness();
    }

    /**
     * Calculate the degree of crowding around a cell.
     * This is the mean crowding of the
     *
     * @param row The row.
     * @param col The column.
     * @return The mean crowdedness of the cell.
     */
    public double calculateMeanNeighborsCrowd(int row, int col) {
        double sum = 0;
        sum += calculateCrowd(row - 1, col - 1);
        sum += calculateCrowd(row - 1, col);
        sum += calculateCrowd(row - 1, col + 1);

        sum += calculateCrowd(row, col - 1);
        sum += calculateCrowd(row, col + 1);

        sum += calculateCrowd(row + 1, col - 1);
        sum += calculateCrowd(row + 1, col);
        sum += calculateCrowd(row + 1, col + 1);

        return sum / 8.0;
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
    public double[] getCellInfoVector(int row, int col) {
        double[] result = new double[CELL_VECTOR_LENGTH];
        PlantUniverseCell cell = getCell(row, col);

        // Height
        result[0] = row / PlantUniverse.UNIVERSE_HEIGHT;
        // Sunlight
        if (row < PlantUniverse.GROUND_LINE) {
            result[1] = cell.getCalculatedSunlight();
        } else {
            result[1] = cell.getCalculatedWater();
        }
        // Crowd
        result[2] = calculateMeanNeighborsCrowd(row, col);
        // Nourishment
        result[3] = cell.getNourishment();
        //


        return result;
    }

    /**
     * Reset the entire grid to a single seed.
     */
    public void reset() {
        for (final PlantUniverseCell[] aGrid : grid) {
            for (PlantUniverseCell cell : aGrid) {
                cell.setLeafyness(0);
                cell.setEnergy(0);
                cell.setNourishment(0);
            }
        }

        int center = PlantUniverse.UNIVERSE_WIDTH / 2;
        int groundLevel = PlantUniverse.GROUND_LINE;

        // root
        grid[groundLevel][center].setLeafyness(0);
        grid[groundLevel][center].setNourishment(1);
        grid[groundLevel][center].setEnergy(1);

        // stem
        grid[groundLevel - 1][center].setLeafyness(0.5);
        grid[groundLevel - 1][center].setNourishment(1);
        grid[groundLevel - 1][center].setEnergy(1);

        // leaf
        grid[groundLevel - 2][center].setLeafyness(1);
        grid[groundLevel - 2][center].setNourishment(1);
        grid[groundLevel - 2][center].setEnergy(1);

    }

    /**
     * Returns true if a cell is valid. Invalid cells are off the bounds of a grid.
     *
     * @param row The row.
     * @param col The column.
     * @return True, if valid.
     */
    public boolean isValid(final int row, final int col) {
        if (row < 0 || col < 0) {
            return false;
        }

        if (row >= this.grid.length) {
            return false;
        }

        if (col >= this.grid[row].length) {
            return false;
        }

        return true;
    }

    /**
     * Calculate the energy for a cell.
     *
     * @param row The row.
     * @param col The column.
     * @return The info vector.
     */
    public double calculateEnergy(final int row, final int col) {
        if (!isValid(row, col)) {
            return 0;
        }
        return this.grid[row][col].getEnergy();
    }

    /**
     * Calculate the transfer energy for a cell.  This is the amount of energy transferred into a cell.
     *
     * @param row The row.
     * @param col The column.
     * @return The amount of energy transferred in.
     */
    public double calculateTransferEnergy(final int row, final int col) {
        double result = 0;
        result = Math.max(result, calculateEnergy(row - 1, col - 1));
        result = Math.max(result, calculateEnergy(row - 1, col));
        result = Math.max(result, calculateEnergy(row - 1, col + 1));
        return result;
    }

    /**
     * Calculate the transfer nourishment for a cell.  This is the amount of nourishment transferred into a cell.
     *
     * @param row The row.
     * @param col The column.
     * @return The amount of energy transferred in.
     */
    public double calculateTransferNourishment(final int row, final int col) {
        double result = 0;
        result = Math.max(result, calculateEnergy(row + 1, col - 1));
        result = Math.max(result, calculateEnergy(row + 1, col));
        result = Math.max(result, calculateEnergy(row + 1, col + 1));
        return result;
    }

    /**
     * @return The amount of nourishment in the roots.
     */
    public double getRootCount() {
        return rootCount;
    }

    /**
     * Set the amount of nourishment in the roots.
     *
     * @param rootCount The root count.
     */
    public void setRootCount(final double rootCount) {
        this.rootCount = rootCount;
    }

    /**
     * @return The amount of leafy material above the surface.
     */
    public double getSurfaceCount() {
        return surfaceCount;
    }

    /**
     * Set the amount of leafy material above the surface.
     *
     * @param surfaceCount The surface count.
     */
    public void setSurfaceCount(final double surfaceCount) {
        this.surfaceCount = surfaceCount;
    }

    /**
     * Count the number of live cells as neighbors to a cell.
     *
     * @param row The row.
     * @param col The column.
     * @return The neighbor count.
     */
    public int countNeighbors(int row, int col) {
        int sum = 0;

        if (isAlive(row - 1, col)) {
            sum++;
        }
        if (isAlive(row + 1, col)) {
            sum++;
        }
        if (isAlive(row, col - 1)) {
            sum++;
        }
        if (isAlive(row, col + 1)) {
            sum++;
        }

        if (isAlive(row - 1, col - 1)) {
            sum++;
        }
        if (isAlive(row + 1, col + 1)) {
            sum++;
        }
        if (isAlive(row - 1, col + 1)) {
            sum++;
        }
        if (isAlive(row + 1, col - 1)) {
            sum++;
        }

        return sum;
    }

    /**
     * Returns true, if the specified cell can grow.
     *
     * @param row The row.
     * @param col The column.
     * @return True, if the specified cell is allowed to grow.
     */
    public boolean canGrow(int row, int col) {
        PlantUniverseCell cell = getCell(row, col);
        if (cell.isAlive()) {
            if (row >= PlantUniverse.GROUND_LINE) {
                return countNeighbors(row, col) < 4;
            } else {
                return cell.getEnergy() > PlantUniverse.GROWTH_THRESHOLD && cell.getNourishment() > PlantUniverse.GROWTH_THRESHOLD;
            }
        }
        return false;
    }

    /**
     * Returns true, if the specified cell is alive. Alive cells have energy.
     *
     * @param row The row.
     * @param col The column.
     * @return True, if the specified cell is alive to grow.
     */
    public boolean isAlive(final int row, final int col) {
        return isValid(row, col) && (grid[row][col].isAlive());

    }
}
