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

    public void initHelper(double[] theData, int theOffset, int theLength) {
        this.data = theData;
        this.offset = theOffset;
        this.length = theLength;
    }
}
