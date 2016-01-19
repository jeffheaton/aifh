package com.heatonresearch.aifh.flat;

/**
 * Created by jeff on 1/19/16.
 */
public class FlatVector extends AbstractFlatObject {

    private int columns;

    public FlatVector(int columns) {
        this.columns = columns;
    }

    /**
     * Setup this object when the structure is finalized.
     *
     * @param theOffset The offset that this object should start at.
     * @return The offset that the next object should start at.
     */
    @Override
    public int init(int theOffset) {
        initHelper(theOffset,columns);
        return theOffset+columns;
    }

    public static FlatVector createSingleVector(int columns) {
        FlatVector result = new FlatVector(columns);
        FlatData holder = new FlatData();
        holder.addFlatObject(result);
        holder.finalizeStructure();
        return result;
    }

    public static FlatObject createSingleVector(double[] d) {
        FlatVector result = new FlatVector(d.length);
        FlatData holder = new FlatData();
        holder.addFlatObject(result);
        holder.finalizeStructure();
        for(int i=0;i<d.length;i++) {
            result.set(i,d[i]);
        }
        return result;
    }
}
