/*
 * Artificial Intelligence for Humans
 * Volume 3: Deep Learning and Neural Networks
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh
 *
 * Copyright 2014-2015 by Jeff Heaton
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
package com.heatonresearch.aifh.flat;

/**
 * A volume is a 3D data object.
 */
public class FlatVolume extends AbstractFlatObject {
    /**
     * The number of rows in the object.
     */
    private int rows;

    /**
     * The number of columns in the object.
     */
    private int columns;

    /**
     * The depth of the object.
     */
    private int depth;

    /**
     * The number of extra elements in the object.  Used to hold a bias neuron, usually.
     */
    private int extra;

    /**
     * Construct the volume.
     * @param theRows The number of rows.
     * @param theColumns The number of columns.
     * @param theDepth The depth.
     * @param hasBias True, if this object will have a bias element.
     */
    public FlatVolume(final int theRows, final int theColumns, final int theDepth, final boolean hasBias) {
        this.rows = theRows;
        this.columns = theColumns;
        this.depth = theDepth;
        if( hasBias ) {
            extra = 1;
        } else {
            extra = 0;
        }
    }

    /**
     * Construct the volume.
     * @param count A 3D array that specifies rows, columns, and depth.  All 3 must be present.
     * @param hasBias True, if this object contains an extra bias element.
     */
    public FlatVolume(final int[] count, final boolean hasBias) {
        this(count[0],count[1],count[2], hasBias);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int init(final int theOffset) {
        int len = (this.rows * this.columns * this.depth)+this.extra;
        initHelper(theOffset,len);
        return theOffset+len;
    }

    /**
     * Get an element using a 3D index.
     * @param row The row to access.
     * @param col The column to access.
     * @param depth The depth to access.
     * @return The value accessed.
     */
    public double get(final int row, final int col, final int depth) {
        int s = this.rows*this.columns;
        int idx = (depth*s)+(row*this.columns)+col;
        return getData()[getOffset()+idx];
    }

    /**
     * Set an element using a 3D index.
     * @param row The row to set.
     * @param col The column to set.
     * @param depth The depth to set.
     * @param v The value to set.
     */
    public void set(final int row, final int col, final int depth, final double v) {
        int s = this.rows*this.columns;
        int idx = (depth*s)+(row*this.columns)+col;
        getData()[getOffset()+idx] = v;
    }


    /**
     * @return The count of extra data.
     */
    public int getExtraCount() {
        return this.extra;
    }

    /**
     * Create a single volumne.
     * @param rows The number of rows.
     * @param columns The number of columns.
     * @param depth The depth.
     * @param bias True if biased.
     * @return The new single volume.
     */
    public static FlatVolume createSingleVolume(int rows, int columns, int depth, boolean bias) {
        FlatVolume result = new FlatVolume(rows,columns,depth, bias);
        FlatData holder = new FlatData();
        holder.addFlatObject(result);
        holder.finalizeStructure();
        return result;
    }

    /**
     * Create a single volume from 1D data.
     * @param data The 1D data.
     * @return The new single volume.
     */
    public static FlatVolume createSingleVolume(double[] data) {
        FlatVolume result = new FlatVolume(data.length,1,1,false);
        FlatData holder = new FlatData();
        holder.addFlatObject(result);
        holder.finalizeStructure();
        System.arraycopy(data,0,holder.getData(),0,data.length);
        return result;
    }
}
