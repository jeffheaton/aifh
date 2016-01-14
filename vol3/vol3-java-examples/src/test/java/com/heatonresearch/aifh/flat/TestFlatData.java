package com.heatonresearch.aifh.flat;

import org.junit.Assert;
import org.junit.Test;

/**
 * Created by Jeff on 1/14/2016.
 */
public class TestFlatData {
    @Test
    public void testBuildFlatData() {
        FlatObject f1,f2,f3,f4;
        FlatData flat = new FlatData();
        flat.addFlatObject(f1 = new FlatMatrix(2,3));
        flat.addFlatObject(f2 = new FlatVolume(3,4,5,false));
        flat.addFlatObject(f3 = new FlatMatrix(4,5));
        flat.addFlatObject(f4 = new FlatVolume(5,6,7,true));
        flat.finalizeStructure();
        Assert.assertEquals(297,flat.getData().length);
        Assert.assertEquals(6,f1.getLength());
        Assert.assertEquals(60,f2.getLength());
        Assert.assertEquals(20,f3.getLength());
        Assert.assertEquals(211,f4.getLength());
        Assert.assertEquals(0,f1.getOffset());
        Assert.assertEquals(6,f2.getOffset());
        Assert.assertEquals(66,f3.getOffset());
        Assert.assertEquals(86,f4.getOffset());
    }
}
