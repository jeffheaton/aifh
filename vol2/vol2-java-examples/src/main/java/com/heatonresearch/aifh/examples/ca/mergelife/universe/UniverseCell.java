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

/**
 * An individual universe cell.  Each cell can have multiple properties.  Typically there are three properties,
 * that represent the three RGB components.
 */
public class UniverseCell {

    /**
     * The properties for this cell.
     */
    private final double[] prop;

    /**
     * Construct a universe cell with the specified number of properties.
     *
     * @param theSize The number of properties.
     */
    public UniverseCell(final int theSize) {
        this.prop = new double[theSize];
    }

    /**
     * Add the specified value to the specified property.
     *
     * @param i The property index.
     * @param d The other value to add.
     */
    public void add(final int i, final double d) {
        this.prop[i] += d;
    }

    /**
     * Add the properties of another cell to this one.
     *
     * @param otherCell The other cell.
     */
    public void add(final UniverseCell otherCell) {
        for (int i = 0; i < this.prop.length; i++) {
            this.prop[i] += otherCell.get(i);
        }
    }

    /**
     * Get a property.
     *
     * @param i The index of the property.
     * @return The property value.
     */
    public double get(final int i) {
        return this.prop[i];
    }

    /**
     * Get an average of the properties.
     *
     * @return The propety average.
     */
    public double getAvg() {
        double result = 0;
        for (final double element : this.prop) {
            result += element;
        }
        return result / this.prop.length;
    }

    /**
     * @return The property array.
     */
    public double[] getData() {
        return this.prop;
    }

    /**
     * Randomize the properties between (-1,1).
     *
     * @param rnd A random number generator.
     */
    public void randomize(GenerateRandom rnd) {
        for (int i = 0; i < this.prop.length; i++) {
            this.prop[i] = rnd.nextDouble(-1, 1);
        }

    }

    /**
     * Set the specified property.
     *
     * @param i The index of the property.
     * @param d The property value.
     */
    public void set(final int i, final double d) {
        this.prop[i] = d;

    }

    /**
     * Set this cell's properties to another cell.
     *
     * @param otherCell The other cell.
     */
    public void set(final UniverseCell otherCell) {
        for (int i = 0; i < this.prop.length; i++) {
            this.prop[i] = otherCell.get(i);
        }
    }

    /**
     * @return The number of properties.
     */
    public int size() {
        return this.prop.length;
    }
}
