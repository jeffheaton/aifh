package com.heatonresearch.aifh.flat;

/**
 * Created by Jeff on 1/9/2016.
 */
public class FlatVolume extends AbstractFlatObject {
    private int rows;
    private int columns;
    private int depth;

    public FlatVolume(final int theRows, final int theColumns, final int theDepth) {
        this.rows = theRows;
        this.columns = theColumns;
        this.depth = theDepth;
    }

    @Override
    public int init(final double[] theData, final int theOffset) {
        int len = this.rows * this.columns * this.depth;
        initHelper(theData,theOffset,len);
        return theOffset+len;
    }
}
