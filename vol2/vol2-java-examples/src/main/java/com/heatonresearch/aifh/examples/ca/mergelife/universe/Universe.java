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
package com.heatonresearch.aifh.examples.ca.mergelife.universe;

import com.heatonresearch.aifh.randomize.GenerateRandom;

public class Universe implements Cloneable {
    /**
     * The cell size.
     */
    private final int cellSize;

    /**
     * The universe.
     */
    private final UniverseCell[][] data;

    /**
     * The constructor.
     *
     * @param height  The universe height.
     * @param width   The universe width.
     * @param theSize The number of dimensions in a universe cell.
     */
    public Universe(final int height, final int width, final int theSize) {
        this.cellSize = theSize;
        this.data = new UniverseCell[height][width];
        for (int row = 0; row < height; row++) {
            for (int col = 0; col < width; col++) {
                this.data[row][col] = new UniverseCell(theSize);
            }
        }
    }

    /**
     * Add the specified value to the specified dimension of the specified cell.
     *
     * @param row The cell's row.
     * @param col The cell's column.
     * @param i   The index of the dimension to add to.
     * @param d   The value to add to the cell.
     */
    public void add(final int row, final int col, final int i, final double d) {
        this.data[row][col].add(i, d);

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Object clone() {
        final Universe result = new Universe(getHeight(), getWidth(),
                this.cellSize);
        result.copy(this);
        return result;
    }

    /**
     * Compare this universe to another and return the difference.  A value of zero indicates an identical universe.
     * The lower the value, the more similar.
     *
     * @param otherUniverse The other universe.
     * @return The difference between the universes.
     */
    public double compare(final Universe otherUniverse) {
        int result = 0;
        int total = 0;
        for (int row = 0; row < otherUniverse.getHeight(); row++) {
            for (int col = 0; col < otherUniverse.getWidth(); col++) {
                final int d1 = Math.abs((int) (255 * get(row, col).getAvg()));
                final int d2 = Math.abs((int) (255 * otherUniverse
                        .get(row, col).getAvg()));
                if (Math.abs(d1 - d2) > 10) {
                    result++;
                }
                total++;
            }
        }

        return (double) result / (double) total;
    }

    /**
     * Copy another universe into this one.
     *
     * @param source The source universe.
     */
    public void copy(final Universe source) {
        for (int row = 0; row < getHeight(); row++) {
            for (int col = 0; col < getWidth(); col++) {
                for (int i = 0; i < this.cellSize; i++) {
                    this.data[row][col].set(i, source.get(row, col).get(i));
                }
            }
        }
    }

    /**
     * Get the universe cell for the specified row and column.
     *
     * @param row The row.
     * @param col The column.
     * @return The universe cell.
     */
    public UniverseCell get(final int row, final int col) {
        return this.data[row][col];
    }

    /**
     * Get the universe cell for the specified row, column and index.
     *
     * @param row The row of the cell.
     * @param col The column of the cell.
     * @param i   The index (dimension) inside the cell.
     * @return The value.
     */
    public double get(final int row, final int col, final int i) {
        return this.data[row][col].get(i);
    }

    /**
     * The number of dimensions inside a cell.
     *
     * @return The size of a cell.
     */
    public int getCellSize() {
        return this.cellSize;
    }

    /**
     * @return The universe grid.
     */
    public UniverseCell[][] getData() {
        return this.data;
    }

    /**
     * @return The height of the universe.
     */
    public int getHeight() {
        return this.data.length;
    }

    /**
     * @return The width of the universe.
     */
    public int getWidth() {
        return this.data[0].length;
    }

    /**
     * Determine if row and col are valid.  Both must be above zero and within the height and width.
     *
     * @param row The row.
     * @param col The column.
     * @return True, if valid.
     */
    public boolean isValid(final int row, final int col) {
        return !(row < 0 || col < 0 || row >= getHeight() || col >= getWidth());
    }

    /**
     * Randomize the universe.
     *
     * @param rnd A random number generator.
     */
    public void randomize(GenerateRandom rnd) {
        for (int row = 0; row < getHeight(); row++) {
            for (int col = 0; col < getWidth(); col++) {
                for (int i = 0; i < 3; i++) {
                    this.data[row][col].randomize(rnd);
                }
            }
        }
    }

}
