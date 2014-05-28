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
package com.heatonresearch.aifh.genetic.genome;

import com.heatonresearch.aifh.evolutionary.genome.BasicGenome;
import com.heatonresearch.aifh.evolutionary.genome.Genome;

/**
 * A genome made up of continuous doubles.
 */
public class DoubleArrayGenome extends BasicGenome implements ArrayGenome {

    /**
     * The serial id.
     */
    private static final long serialVersionUID = 1L;

    /**
     * The data.
     */
    private double[] data;

    /**
     * Construct a genome of a specific size.
     *
     * @param size The size.
     */
    public DoubleArrayGenome(int size) {
        this.data = new double[size];
    }

    /**
     * Construct a genome based on another genome.
     *
     * @param other The other genome.
     */
    public DoubleArrayGenome(DoubleArrayGenome other) {
        this.data = other.getData().clone();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int size() {
        return this.data.length;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void copy(ArrayGenome source, int sourceIndex, int targetIndex) {
        DoubleArrayGenome sourceInt = (DoubleArrayGenome) source;
        this.data[targetIndex] = sourceInt.data[sourceIndex];

    }

    /**
     * @return The data.
     */
    public double[] getData() {
        return this.data;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double[] getLongTermMemory() {
        return this.data;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void copy(Genome source) {
        DoubleArrayGenome sourceDouble = (DoubleArrayGenome) source;
        System.arraycopy(sourceDouble.data, 0, this.data, 0, this.data.length);
        setScore(source.getScore());
        setAdjustedScore(source.getAdjustedScore());

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void swap(int iswap1, int iswap2) {
        double temp = this.data[iswap1];
        this.data[iswap1] = this.data[iswap2];
        this.data[iswap2] = temp;

    }

}
