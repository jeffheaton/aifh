/*
 * Artificial Intelligence for Humans
 * Volume 1: Fundamental Algorithms
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh

 * Copyright 2013 by Jeff Heaton
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * For more information on Heaton Research copyrights, licenses
 * and trademarks visit:
 * http://www.heatonresearch.com/copyright
 */

package com.heatonresearch.aifh.randomize;

/**
 * Test secure random.
 */
public class TestSecureGenerateRandom {

    @org.junit.Test
    public void testGenerateBoolean() {
        final SecureGenerateRandom rnd = new SecureGenerateRandom(1);
        rnd.nextBoolean();
    }

    @org.junit.Test
    public void testDoubleRange() {
        final SecureGenerateRandom rnd = new SecureGenerateRandom(1);
        rnd.nextDouble(-1, 1);
    }

    @org.junit.Test
    public void testDouble() {
        final SecureGenerateRandom rnd = new SecureGenerateRandom(1);
        rnd.nextDouble();
    }

    @org.junit.Test
    public void testLong() {
        final SecureGenerateRandom rnd = new SecureGenerateRandom(1);
        rnd.nextLong();
    }

    @org.junit.Test
    public void testFloat() {
        final SecureGenerateRandom rnd = new SecureGenerateRandom(1);
        rnd.nextFloat();
    }

    @org.junit.Test
    public void testGaussianFloat() {
        final SecureGenerateRandom rnd = new SecureGenerateRandom(1);
        rnd.nextGaussian();
    }

    @org.junit.Test
    public void testInt() {
        final SecureGenerateRandom rnd = new SecureGenerateRandom(1);
        rnd.nextInt();
    }

    @org.junit.Test
    public void testIntRange() {
        final SecureGenerateRandom rnd = new SecureGenerateRandom(1);
        rnd.nextInt(0, 10);
    }
}
