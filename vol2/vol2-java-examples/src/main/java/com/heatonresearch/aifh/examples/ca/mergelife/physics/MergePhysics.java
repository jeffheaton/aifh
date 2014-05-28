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
package com.heatonresearch.aifh.examples.ca.mergelife.physics;

import com.heatonresearch.aifh.examples.ca.mergelife.universe.Universe;
import com.heatonresearch.aifh.examples.ca.mergelife.universe.UniverseCell;

import java.io.*;
import java.util.Arrays;
import java.util.StringTokenizer;

/**
 * Merge physics determines how cells are to be populated.
 * <p/>
 * For more information on how the physics works see:
 * <p/>
 * http://www.codeproject.com/Articles/730362/Using-an-evolutionary-algorithm-to-create-a-cellul
 */
public class MergePhysics implements Physics {

    /**
     * The color table.
     */
    private final double[][] colorTable = {{-1, -1, -1}, {1, -1, -1},
            {-1, 1, -1}, {1, 1, -1}, {-1, -1, 1}, {1, -1, 1},
            {-1, 1, 1}, {1, 1, 1}};


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
     * The data that defines the physical constants.
     */
    private final double[] data;

    /**
     * The data sorted.
     */
    private final int[] dataOrder;

    /**
     * The universe.
     */
    private final Universe universe;

    /**
     * Construct the merge physics.
     *
     * @param theUniverse The universe.
     */
    public MergePhysics(final Universe theUniverse) {
        this.data = new double[2 * this.colorTable.length];
        this.dataOrder = new int[this.colorTable.length];
        this.universe = theUniverse;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void copyData(final double[] sourceData) {
        System.arraycopy(sourceData, 0, this.data, 0, this.data.length);
        sortData();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double[] getData() {
        return this.data;
    }

    /**
     * Determine if "value" is in the array.
     *
     * @param list  The list.
     * @param len   The length.
     * @param value The value.
     * @return True, if value is in the list.
     */
    private boolean listContains(final int[] list, final int len,
                                 final int value) {
        for (int i = 0; i < len; i++) {
            if (list[i] == value) {
                return true;
            }
        }
        return false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void load(final String filename) throws IOException {
        final BufferedReader br = new BufferedReader(new FileReader(filename));
        try {
            String line = br.readLine();
            // strip off leading and training braces
            line = line.substring(1);
            line = line.substring(0, line.length() - 1);
            final StringTokenizer tok = new StringTokenizer(line, ",c");
            int idx = 0;
            while (tok.hasMoreElements()) {
                this.data[idx++] = Double.parseDouble(tok.nextToken());
                sortData();
            }
        } finally {
            br.close();
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void processPixel(final Universe outputUniverse, final int row,
                             final int col) {
        double total = 0;
        int cnt = 0;

        for (int dir = 0; dir < this.rowTransform.length; dir++) {
            final int otherRow = row + this.rowTransform[dir];
            final int otherCol = col + this.colTransform[dir];
            if (this.universe.isValid(otherRow, otherCol)) {
                final UniverseCell otherCell = this.universe.get(otherRow,
                        otherCol);
                total += otherCell.getAvg();
                cnt++;
            }
        }

        total /= cnt;
        for (int i = 0; i < this.colorTable.length; i++) {
            final int idx = this.dataOrder[i];
            if (total < this.data[idx * 2]) {
                for (int j = 0; j < outputUniverse.getCellSize(); j++) {
                    double d = this.colorTable[idx][j]
                            - this.universe.get(row, col, j);
                    double pct = this.data[1 + idx * 2];
                    pct = (pct + 1.0) / 2.0;
                    d *= pct;
                    outputUniverse.add(row, col, j, d);
                }
                break;
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void randomize() {
        for (int i = 0; i < this.data.length; i++) {
            this.data[i] = Math.random() * 2.0 - 1.0;
        }
        sortData();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void save(final String filename) throws IOException {
        final BufferedWriter out = new BufferedWriter(new FileWriter(filename));
        out.write(Arrays.toString(this.data));
        out.newLine();
        out.close();
    }

    /**
     * Sort the physical data for faster lookup.
     */
    private void sortData() {
        // create the order table
        for (int x = 0; x < this.colorTable.length; x++) {
            int lowestIndex = -1;
            for (int y = 0; y < this.colorTable.length; y++) {
                final double value = this.data[y * 2];
                // Only consider positions that are not already in the list.
                if (!listContains(this.dataOrder, x, y)) {
                    // See if this is the lowest index found for this pass.
                    if (lowestIndex == -1) {
                        lowestIndex = y;
                    } else {
                        if (value < this.data[lowestIndex * 2]) {
                            lowestIndex = y;
                        }
                    }
                }
            }
            this.dataOrder[x] = lowestIndex;
        }
    }
}
