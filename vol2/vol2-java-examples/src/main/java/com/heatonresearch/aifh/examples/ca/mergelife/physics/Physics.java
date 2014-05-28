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

import java.io.IOException;

/**
 * Defines a set of "physics" rules to control a cellular automation.
 */
public interface Physics {
    /**
     * Copy the physics constants vector from another array.
     *
     * @param sourceData The source vector to copy.
     */
    void copyData(double[] sourceData);

    /**
     * @return Get the physical constants vector.
     */
    double[] getData();

    /**
     * Load the physical constants vector from a text file.
     *
     * @param filename The filename.
     * @throws IOException
     */
    void load(String filename) throws IOException;

    /**
     * Save the physical constants vector to a text file.
     *
     * @param filename The filename.
     * @throws IOException
     */
    void save(String filename) throws IOException;

    /**
     * Perform the actual physics.
     *
     * @param outputUniverse The new output universe.
     * @param row            The row of the cell we are processing.
     * @param col            The column of the cell we are processing.
     */
    void processPixel(Universe outputUniverse, int row, int col);

    /**
     * Randomize the physics to random values.
     */
    void randomize();
}
