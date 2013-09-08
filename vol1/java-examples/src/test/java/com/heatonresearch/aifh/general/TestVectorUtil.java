package com.heatonresearch.aifh.general;

import com.heatonresearch.aifh.AIFH;
import org.junit.Test;

import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

/**
 * Test the vector utility.
 */
public class TestVectorUtil {
    @Test
    public void testMaxIndex() {
        double[] a = {2, 4, 10, 8};
        assertEquals(2, VectorUtil.maxIndex(a), AIFH.DEFAULT_PRECISION);
    }

    @Test
    public void testConstructorIsPrivate() throws Exception {
        Constructor constructor = VectorUtil.class.getDeclaredConstructor();
        assertTrue(Modifier.isPrivate(constructor.getModifiers()));
        constructor.setAccessible(true);
        constructor.newInstance();
    }
}
