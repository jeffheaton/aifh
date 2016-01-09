package com.heatonresearch.aifh.flat;

/**
 * Created by Jeff on 1/9/2016.
 */
public interface FlatObject {
    double[] getFlatDataArray();
    int getOffset();
    int getLength();
    int init(final int theOffset);
    void setData(final double[] theData);
}
