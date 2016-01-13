package com.heatonresearch.aifh.flat;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by Jeff on 1/9/2016.
 */
public class FlatData {
    private double[] data;
    private final List<FlatObject> flatObjects = new ArrayList<FlatObject>();

    public double[] getData() {
        return this.data;
    }

    public List<FlatObject> getFlatObjects() {
        return this.flatObjects;
    }

    public void addFlatObject(final FlatObject theFlatObject) {
        this.flatObjects.add(theFlatObject);
    }

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

    public void clear() {
        for(int i=0;i<this.data.length;i++) {
            this.data[i] = 0;
        }
    }
}
