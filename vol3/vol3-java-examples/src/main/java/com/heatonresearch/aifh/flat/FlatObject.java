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
 * Defines a flat object.  This is usually a vector, matrix or volume, that can be mapped into a single vector.
 * This allows a nueral network structure that consists of matrixes to be represented as a single array.
 */
public interface FlatObject {

    /**
     * @return The offset to this object in the master array.
     */
    int getOffset();

    /**
     * @return The length of this object in the master array.
     */
    int getLength();

    /**
     * Setup this object when the structure is finalized.
     * @param theOffset The offset that this object should start at.
     * @return The offset that the next object should start at.
     */
    int init(final int theOffset);

    /**
     * Set a reference to the master array.
     * @param theData The master array.
     */
    void setData(final double[] theData);

    /**
     * Get an element of the object using 1D indexing.
     * @param index The index to use.
     * @return The number at the specified index.
     */
    double get(final int index);

    /**
     * Set an element of the object, using 1D indexing.
     * @param index The index.
     * @param d The value to set.
     */
    void set(final int index, final double d);

    /**
     * Add to an element of the object, using 1D indexing.
     * @param index The index.
     * @param d The value to add.
     */
    void add(final int index, final double d);

    /**
     * Copy one flat object to another.  The two objects do not need to be of the same type, but they must be of the
     * same length.
     * @param target The target object.
     */
    void copyTo(FlatObject target);

    /**
     * Copy the flat object to an array.  Array must match the size of the flat object.
     * @param array The target array.
     * @param offset The target offset to start at.
     */
    void copyTo(double[] array, int offset);

    /**
     * Create a 1D array that holds the data from this object.
     * @return The new array.
     */
    double[] extractArray();
}
