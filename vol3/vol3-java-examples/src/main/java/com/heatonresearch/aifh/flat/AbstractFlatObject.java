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

/**
 * Provides some basic functionality for flat objects.  This class does not need to be used by all flat objects.
 * This class assumes that the flat object has a length and offset that must be stored.  Additionally,
 * a reference to the master array is kept.
 */
public abstract class AbstractFlatObject implements FlatObject {

    /**
     * Regerence to the master array.
     */
    private double[] data;

    /**
     * The offset into the master array that this object begins at.
     */
    private int offset;

    /**
     * The length of this data object in the flat array.
     */
    private int length;


    /**
     * {@inheritDoc}
     */
    @Override
    public int getOffset() {
        return this.offset;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getLength() {
        return this.length;
    }

    /**
     * Called by child objects to init the offset and length when the master array is created.
     * @param theOffset The offset that this object will be at.
     * @param theLength The length of this object.
     */
    public void initHelper(final int theOffset, final int theLength) {
        this.offset = theOffset;
        this.length = theLength;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setData(double[] theData) {
        this.data = theData;
    }

    /**
     * @return The master array.
     */
    public double[] getData() {
        return this.data;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double get(final int index) {
        return this.data[this.offset+index];
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void set(final int index, final double d) {
        if( index>=this.length ) {
            throw new AIFHError("Length ("+this.length+") exceeded: "+index);
        }
        this.data[this.offset+index] = d;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void add(final int index, final double d) {
        this.data[this.offset+index] += d;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        StringBuilder result = new StringBuilder();
        result.append("[");
        result.append(this.getClass().getSimpleName());
        result.append(":offset=");
        result.append(getOffset());
        result.append(",len=");
        result.append(getLength());
        result.append(";");
        for(int i=0;i<getLength();i++) {
            if(i!=0) {
                result.append(",");
            }
            result.append(this.get(i));
        }

        result.append("]");
        return result.toString();
    }

    public double[] extractArray() {
        double[] result = new double[getLength()];
        for(int i=0;i<getLength();i++) {
            result[i] = get(i);
        }
        return result;
    }

    /**
     * {@inheritDoc}
     */
    public void copyTo(FlatObject target) {
        if( getLength()>target.getLength()) {
            throw new AIFHError("Can't copy between two FlatObject's that do not have matching lengths.");
        }

        for(int i=0;i<getLength();i++) {
            target.set(i,get(i));
        }
    }

    /**
     * {@inheritDoc}
     */
    public void copyTo(double[] target, int offset) {
        if( getLength()>target.length) {
            throw new AIFHError("Can't copy between a FlatObject and an array that do not have matching lengths.");
        }

        for(int i=0;i<getLength();i++) {
            target[i+offset] = get(i);
        }
    }
}
