package com.heatonresearch.aifh.flat;

import com.heatonresearch.aifh.AIFH;
import org.junit.Assert;
import org.junit.Test;

import java.util.Arrays;

/**
 * Created by Jeff on 1/14/2016.
 */
public class TestFlatVolume {

    @Test
    public void TestMatrixAccess3D() {
        FlatData flat = new FlatData();
        FlatMatrix spacer = new FlatMatrix(2, 2);
        FlatVolume testVolume = new FlatVolume(2, 3, 4, false);
        flat.addFlatObject(spacer);
        flat.addFlatObject(testVolume);
        flat.finalizeStructure();

        for (int row = 0; row < 2; row++) {
            for (int col = 0; col < 3; col++) {
                for (int depth = 0; depth < 4; depth++) {
                    int idx = (depth * 100) + (row * 10) + col;
                    testVolume.set(row, col, depth, idx);
                }
            }
        }

        for (int row = 0; row < 2; row++) {
            for (int col = 0; col < 3; col++) {
                for (int depth = 0; depth < 4; depth++) {
                    int idx = (depth * 100) + (row * 10) + col;
                    Assert.assertEquals(idx,testVolume.get(row, col, depth), AIFH.DEFAULT_PRECISION);
                }
            }
        }

        double[] flatCheck = {
                0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 2.0, 10.0, 11.0, 12.0, 100.0, 101.0, 102.0, 110.0, 111.0,
                112.0, 200.0, 201.0, 202.0, 210.0, 211.0, 212.0, 300.0, 301.0, 302.0, 310.0, 311.0, 312.0
        };

        Assert.assertArrayEquals(flatCheck,flat.getData(),AIFH.DEFAULT_PRECISION);
    }

    @Test
    public void TestMatrixAccess1D() {
        FlatData flat = new FlatData();
        FlatMatrix spacer = new FlatMatrix(2, 2);
        FlatVolume testVolume = new FlatVolume(2, 3, 4, false);
        flat.addFlatObject(spacer);
        flat.addFlatObject(testVolume);
        flat.finalizeStructure();

        for(int i=0;i<testVolume.getLength();i++) {
            testVolume.set(i,i);
        }

        for(int i=0;i<testVolume.getLength();i++) {
            Assert.assertEquals(i,(int)testVolume.get(i));
        }

        double[] flatTest = {
                0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0,
                11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0, 20.0, 21.0, 22.0, 23.0};
        Assert.assertArrayEquals(flatTest,flat.getData(),AIFH.DEFAULT_PRECISION);

        for(int i=0;i<4;i++) {
            int d = i*6;
            Assert.assertEquals(0+d, testVolume.get(0, 0, i), AIFH.DEFAULT_PRECISION);
            Assert.assertEquals(1+d, testVolume.get(0, 1, i), AIFH.DEFAULT_PRECISION);
            Assert.assertEquals(2+d, testVolume.get(0, 2, i), AIFH.DEFAULT_PRECISION);

            Assert.assertEquals(3+d, testVolume.get(1, 0, i), AIFH.DEFAULT_PRECISION);
            Assert.assertEquals(4+d, testVolume.get(1, 1, i), AIFH.DEFAULT_PRECISION);
            Assert.assertEquals(5+d, testVolume.get(1, 2, i), AIFH.DEFAULT_PRECISION);
        }
    }
}

