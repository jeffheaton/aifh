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

import com.heatonresearch.aifh.AIFHError;
import com.heatonresearch.aifh.util.NumberFormatting;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * A FlatData object is a 1D array of doubles that allows matrixes, volumes and vectors to be mapped into it.  This
 * allows a neural network structure to be represented as a 1D vector array.  Data objects that are to be mapped
 * can be added to this class.  The flat array is not constructued until the finalizeStructure method is calle=d.
 */
public class FlatData {

    /**
     * The master array.  This is the 1D vector that the data objects will be mapped into.
     */
    private double[] data;

    /**
     * The list of objects that are mapped into the master array.
     */
    private final List<FlatObject> flatObjects = new ArrayList<FlatObject>();

    /**
     * @return The master array that the objects are mapped into.
     */
    public double[] getData() {
        return this.data;
    }

    /**
     * @return The list of objects that are mapped into the master array.
     */
    public List<FlatObject> getFlatObjects() {
        return this.flatObjects;
    }

    /**
     * Add the specified flat object.  Do not use until all objects are added, and finalizeStructure is called.
     * @param theFlatObject The object to add.
     */
    public void addFlatObject(final FlatObject theFlatObject) {
        this.flatObjects.add(theFlatObject);
    }

    /**
     * Finalize the structure and build the master array.  Once this method is called, no more flat objects can be
     * added.  The previously added objects can now be used.
     */
    public void finalizeStructure() {
        int offset = 0;
        for(FlatObject obj: this.flatObjects) {
            offset = obj.init(offset);
        }

        this.data = new double[offset];

        for(FlatObject obj: this.flatObjects) {
            obj.setData(data);
        }
    }

    /**
     * Set the entire master array to zero.
     */
    public void clear() {
        for(int i=0;i<this.data.length;i++) {
            this.data[i] = 0;
        }
    }

    /**
     * @return  The length of the data.
     */
    public int getLength() {
        return this.data.length;
    }

    /**
     * @return The number of flat objects.
     */
    public int flatObjectCount() {
        return this.flatObjects.size();
    }

    /**
     * Get the specified flat object by index.
     * @param idx The flat object.
     * @return The flat object.
     */
    public FlatObject get(int idx) {
        return this.flatObjects.get(idx);
    }

    /**
     * Inverse the order of the objects before the structure is finalized.
     */
    public void reverseOrder() {
        if( this.data!=null ) {
            throw new AIFHError("Can't reverse the order after finalizeStructure was called.");
        }

        Collections.reverse(this.flatObjects);
    }

    /**
     * Convert a flat object to a printable list of numbers.
     * Output will be comma separated and in USA formatting.
     * @param obj The object.
     * @return The string result.
     */
    public static String flatObjectToList(FlatObject obj) {
        StringBuilder result = new StringBuilder('[');
        for(int i=0; i<obj.getLength();i++) {
            if( i!=0 ) {
                result.append(',');
            }
            result.append(NumberFormatting.double2USANUmber(obj.get(i)));
        }
        result.append(']');

        return result.toString();
    }
}
