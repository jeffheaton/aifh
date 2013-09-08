package com.heatonresearch.aifh;

import org.junit.Test;

import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;

import static org.junit.Assert.assertTrue;

/**
 * Test the main AIFH class.
 */
public class TestAIFH {
    @Test
    public void testConstructorIsPrivate() throws Exception {
        Constructor constructor = AIFH.class.getDeclaredConstructor();
        assertTrue(Modifier.isPrivate(constructor.getModifiers()));
        constructor.setAccessible(true);
        constructor.newInstance();
    }
}
