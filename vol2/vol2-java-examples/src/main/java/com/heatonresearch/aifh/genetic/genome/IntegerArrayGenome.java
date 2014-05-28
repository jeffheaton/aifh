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
 * A genome that is an array of discrete integer values.
 */
public class IntegerArrayGenome extends BasicGenome implements ArrayGenome {


    /**
     * The serial id.
     */
    private static final long serialVersionUID = 1L;

    /**
     * The genome data.
     */
    private int[] data;

    /**
     * Construct the genome.
     *
     * @param size The size of the genome.
     */
    public IntegerArrayGenome(int size) {
        this.data = new int[size];
    }

    /**
     * Construct the genome by copying another.
     *
     * @param other The other genome.
     */
    public IntegerArrayGenome(IntegerArrayGenome other) {
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
        IntegerArrayGenome sourceInt = (IntegerArrayGenome) source;
        this.data[targetIndex] = sourceInt.data[sourceIndex];

    }

    public int[] getData() {
        return this.data;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void copy(Genome source) {
        IntegerArrayGenome sourceInt = (IntegerArrayGenome) source;
        System.arraycopy(sourceInt.data, 0, this.data, 0, this.data.length);
        this.setScore(source.getScore());
        this.setAdjustedScore(source.getAdjustedScore());

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void swap(int iswap1, int iswap2) {
        int temp = this.data[iswap1];
        this.data[iswap1] = this.data[iswap2];
        this.data[iswap2] = temp;

    }

    /**
     * Not supported.  Integer files are not generally used for model fitting.
     *
     * @return Nothing.
     */
    @Override
    public double[] getLongTermMemory() {
        throw new UnsupportedOperationException();
    }

}
