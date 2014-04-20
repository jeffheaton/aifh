/*
 * Artificial Intelligence for Humans
 * Volume 2: Nature Inspired Algorithms
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh

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
package com.heatonresearch.aifh.examples.alife.mergelife.universe;

public class UniverseCell {
    private final double[] prop;

    public UniverseCell(final int theSize) {
        this.prop = new double[theSize];
    }

    public UniverseCell(final int theSize, final int theIndex, final double[] d) {
        this(theSize);
        System.arraycopy(d, theIndex + 0, this.prop, 0, theSize);
    }

    public void add(final int i, final double d) {
        this.prop[i] += d;
    }

    public void add(final UniverseCell otherCell) {
        for (int i = 0; i < this.prop.length; i++) {
            this.prop[i] += otherCell.get(i);
        }
    }

    public void clamp(final double low, final int high) {
        for (int i = 0; i < this.prop.length; i++) {
            clamp(i, low, high);
        }

    }

    public void clamp(final int i, final double low, final double high) {
        if (this.prop[i] < low) {
            this.prop[i] = low;
        }
        if (this.prop[i] > high) {
            this.prop[i] = high;
        }
    }

    public double get(final int i) {
        return this.prop[i];
    }

    public double getAvg() {
        double result = 0;
        for (final double element : this.prop) {
            result += element;
        }
        return result / this.prop.length;
    }

    public double[] getData() {
        return this.prop;
    }

    public void multiply(final UniverseCell otherCell) {
        for (int i = 0; i < this.prop.length; i++) {
            this.prop[i] *= otherCell.get(i);
        }
    }

    public void randomize() {
        for (int i = 0; i < this.prop.length; i++) {
            this.prop[i] = Math.random() * 2.0 - 1;
        }

    }

    public void set(final int i, final double d) {
        this.prop[i] = d;

    }

    public void set(final UniverseCell otherCell) {
        for (int i = 0; i < this.prop.length; i++) {
            this.prop[i] = otherCell.get(i);
        }
    }

    public int size() {
        return this.prop.length;
    }
}
