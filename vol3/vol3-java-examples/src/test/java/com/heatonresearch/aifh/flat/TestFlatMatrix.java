package com.heatonresearch.aifh.flat;

import com.heatonresearch.aifh.AIFH;
import org.junit.Assert;
import org.junit.Test;

import java.util.Arrays;

/**
 * Created by Jeff on 1/14/2016.
 */
public class TestFlatMatrix {
    @Test
    public void TestMatrixAccess2D() {
        FlatData flat = new FlatData();
        FlatMatrix spacer = new FlatMatrix(2,2);
        FlatMatrix testMatrix = new FlatMatrix(2,3);
        flat.addFlatObject(spacer);
        flat.addFlatObject(testMatrix);
        flat.finalizeStructure();

        Assert.assertEquals(4,testMatrix.getOffset());
        Assert.assertEquals(6,testMatrix.getLength());

        for(int row=0;row<2;row++) {
            for(int col=0;col<3;col++) {
                testMatrix.set(row,col, (row*10.0)+col);
            }
        }

        for(int row=0;row<2;row++) {
            for(int col=0;col<3;col++) {
                int idx = (row*10)+col;
                Assert.assertEquals((double)idx,testMatrix.get(row,col), AIFH.DEFAULT_PRECISION);
            }
        }

        final double[] flatCheck = { 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 2.0, 10.0, 11.0, 12.0 };

        Assert.assertArrayEquals(flatCheck,flat.getData(),AIFH.DEFAULT_PRECISION);

    }

    @Test
    public void TestMatrixAccess1D() {
        FlatData flat = new FlatData();
        FlatMatrix spacer = new FlatMatrix(2, 2);
        FlatMatrix testMatrix = new FlatMatrix(2, 3);
        flat.addFlatObject(spacer);
        flat.addFlatObject(testMatrix);
        flat.finalizeStructure();

        Assert.assertEquals(4, testMatrix.getOffset());
        Assert.assertEquals(6, testMatrix.getLength());

        for(int i=0;i<testMatrix.getLength();i++) {
            testMatrix.set(i,(double)i);
        }

        for(int i=0;i<testMatrix.getLength();i++) {
            Assert.assertEquals( (double)i, testMatrix.get(i), AIFH.DEFAULT_PRECISION );
        }

        Assert.assertEquals(0,(int)testMatrix.get(0,0));
        Assert.assertEquals(1,(int)testMatrix.get(0,1));
        Assert.assertEquals(2,(int)testMatrix.get(0,2));
        Assert.assertEquals(3,(int)testMatrix.get(1,0));
        Assert.assertEquals(4,(int)testMatrix.get(1,1));
        Assert.assertEquals(5,(int)testMatrix.get(1,2));
    }
}
