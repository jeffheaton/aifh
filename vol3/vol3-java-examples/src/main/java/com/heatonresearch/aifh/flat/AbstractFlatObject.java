package com.heatonresearch.aifh.flat;

/**
 * Created by Jeff on 1/9/2016.
 */
public abstract class AbstractFlatObject implements FlatObject {

    private double[] data;
    private int offset;
    private int length;

    @Override
    public double[] getFlatDataArray() {
        return this.data;
    }

    @Override
    public int getOffset() {
        return this.offset;
    }

    @Override
    public int getLength() {
        return 0;
    }

    public void initHelper(final int theOffset, final int theLength) {
        this.offset = theOffset;
        this.length = theLength;
    }

    @Override
    public void setData(double[] theData) {
        this.data = theData;
    }

    @Override
    public double[] getData() {
        return this.data;
    }

    @Override
    public double get(final int index) {
        return this.data[this.offset+index];
    }

    @Override
    public void set(final int index, final double d) {
        this.data[this.offset+index] = d;
    }

    @Override
    public void add(final int index, final double d) {
        this.data[this.offset+index] += d;
    }
}
