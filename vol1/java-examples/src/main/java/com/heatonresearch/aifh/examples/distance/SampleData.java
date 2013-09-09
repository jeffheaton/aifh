/*
 * Artificial Intelligence for Humans
 * Volume 1: Fundamental Algorithms
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh

 * Copyright 2013 by Jeff Heaton
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

package com.heatonresearch.aifh.examples.distance;

/**
 * SampleData: Holds sampled data that will be used to train the neural network.
 */
public class SampleData implements Comparable<SampleData>, Cloneable {

    /**
     * The downsampled data as a grid of booleans.
     */
    protected final boolean[][] grid;

    /**
     * The letter.
     */
    protected char letter;

    /**
     * The constructor
     *
     * @param letter What letter this is
     * @param width  The width
     * @param height The height
     */
    public SampleData(final char letter, final int width, final int height) {
        this.grid = new boolean[width][height];
        this.letter = letter;
    }

    /**
     * Clear the downsampled image
     */
    public void clear() {
        for (int x = 0; x < this.grid.length; x++) {
            for (int y = 0; y < this.grid[0].length; y++) {
                this.grid[x][y] = false;
            }
        }
    }

    /**
     * Create a copy of this sample
     *
     * @return A copy of this sample
     */
    @Override
    public Object clone()

    {
        final SampleData obj = new SampleData(this.letter, getWidth(),
                getHeight());
        for (int y = 0; y < getHeight(); y++) {
            for (int x = 0; x < getWidth(); x++) {
                obj.setData(x, y, getData(x, y));
            }
        }
        return obj;
    }

    /**
     * Compare this sample to another, used for sorting.
     *
     * @param o The object being compared against.
     * @return Same as String.compareTo
     */

    public int compareTo(final SampleData o) {
        if (getLetter() > o.getLetter()) {
            return 1;
        } else {
            return -1;
        }
    }

    /**
     * Get a pixel from the sample.
     *
     * @param x The x coordinate
     * @param y The y coordinate
     * @return The requested pixel
     */
    public boolean getData(final int x, final int y) {
        return this.grid[x][y];
    }

    /**
     * Get the height of the down sampled image.
     *
     * @return The height of the downsampled image.
     */
    public int getHeight() {
        return this.grid[0].length;
    }

    /**
     * Get the letter that this sample represents.
     *
     * @return The letter that this sample represents.
     */
    public char getLetter() {
        return this.letter;
    }

    /**
     * Get the width of the downsampled image.
     *
     * @return The width of the downsampled image
     */
    public int getWidth() {
        return this.grid.length;
    }

    /**
     * Set one pixel of sample data.
     *
     * @param x The x coordinate
     * @param y The y coordinate
     * @param v The value to set
     */
    public void setData(final int x, final int y, final boolean v) {
        this.grid[x][y] = v;
    }

    /**
     * Set the letter that this sample represents.
     *
     * @param letter The letter that this sample represents.
     */
    public void setLetter(final char letter) {
        this.letter = letter;
    }

    /**
     * Convert this sample to a string.
     *
     * @return Just returns the letter that this sample is assigned to.
     */
    @Override
    public String toString() {
        return "" + this.letter;
    }

    public double[] getPosition() {
        final double[] result = new double[this.grid.length * this.grid[0].length];
        int index = 0;
        for (final boolean[] aGrid : this.grid) {
            for (final boolean anAGrid : aGrid) {
                result[index++] = anAGrid ? 1.0 : -1.0;
            }
        }
        return result;
    }

}
