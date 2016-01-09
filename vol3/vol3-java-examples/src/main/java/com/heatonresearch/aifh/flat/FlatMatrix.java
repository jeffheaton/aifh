package com.heatonresearch.aifh.flat;

/**
 * Created by Jeff on 1/9/2016.
 */
public class FlatMatrix extends AbstractFlatObject {
    private int rows;
    private int columns;

    public FlatMatrix(final int rows, final int columns) {
        this.rows = rows;
        this.columns = columns;
    }

    @Override
    public int init(final double[] theData, final int theOffset) {
        int len = rows * columns;
        initHelper(theData,theOffset,len);
        return theOffset+len;
    }
}
