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

    public FlatVolume(final int[] count) {
        this(count[0],count[1],count[2]);
    }

    @Override
    public int init(final int theOffset) {
        int len = this.rows * this.columns * this.depth;
        initHelper(theOffset,len);
        return theOffset+len;
    }

    public double get(final int row, final int col, final int depth) {
        int s = this.rows*this.columns;
        int idx = (depth*s)+(row*this.columns)+col;
        return getData()[getOffset()+idx];
    }

    public void set(final int row, final int col, final int depth, final double v) {
        int s = this.rows*this.columns;
        int idx = (depth*s)+(row*this.columns)+col;
        getData()[getOffset()+idx] = v;
    }

}
