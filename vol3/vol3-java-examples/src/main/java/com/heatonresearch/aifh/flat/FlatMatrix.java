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
 * A 2D matrix that can be mapped to a flat array.
 */
public class FlatMatrix extends AbstractFlatObject {
    /**
     * The number of rows in the matrix.
     */
    private int rows;

    /**
     * The number of columns in the matrix.
     */
    private int columns;

    /**
     * Construct the matrix.
     * @param rows The number of rows.
     * @param columns The number of columns.
     */
    public FlatMatrix(final int rows, final int columns) {
        this.rows = rows;
        this.columns = columns;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int init(final int theOffset) {
        int len = rows * columns;
        initHelper(theOffset,len);
        return theOffset+len;
    }

    /**
     * Get a value from the matrix.
     * @param row The row.
     * @param col The column.
     * @return The value.
     */
    public double get(final int row, final int col) {
        return getData()[getOffset()+(row * this.columns)+col];
    }

    /**
     * Set a value in the matrix.
     * @param row The row.
     * @param col The column.
     * @param d The value.
     */
    public void set(final int row, final int col, final double d) {
        getData()[getOffset()+(row * this.columns)+col]=d;
    }

    /**
     * Add a value to the matrix.
     * @param row The row to add.
     * @param col The column to add.
     * @param d The value to add to the matrix.
     */
    public void add(final int row, final int col, final double d) {
        getData()[getOffset()+(row * this.columns)+col]+=d;
    }
}
