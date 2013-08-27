package com.heatonresearch.aifh;

import org.junit.Test;

import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;

import static org.junit.Assert.assertTrue;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 8/27/13
 * Time: 5:45 AM
 * To change this template use File | Settings | File Templates.
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
