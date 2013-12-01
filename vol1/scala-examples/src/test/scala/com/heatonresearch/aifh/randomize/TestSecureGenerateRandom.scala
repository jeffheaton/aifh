/*
 * Artificial Intelligence for Humans
 * Volume 1: Fundamental Algorithms
 * Scala Version
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
package com.heatonresearch.aifh.randomize

import org.scalatest.Suite

/**
 * Test secure random.
 * Basically validate they do not exception, we cannot test the return values effectively.
 */
class TestSecureGenerateRandom extends Suite {

  def testDefaultConstructor() {
    val rnd = new SecureGenerateRandom()
    rnd.nextBoolean
  }

  def testGenerateBoolean() {
    val rnd = new SecureGenerateRandom(1)
    rnd.nextBoolean
  }

  def testDoubleRange() {
    val rnd  = new SecureGenerateRandom(1)
    rnd.nextDouble(-1, 1)
  }

  def testDouble() {
    val rnd  = new SecureGenerateRandom(1)
    rnd.nextDouble()
  }

  def testLong() {
    val rnd = new SecureGenerateRandom(1)
    rnd.nextLong
  }

  def testFloat() {
    val rnd = new SecureGenerateRandom(1)
    rnd.nextFloat
  }

  def testGaussianFloat() {
    val rnd = new SecureGenerateRandom(1)
    rnd.nextGaussian
  }

  def testInt() {
    val rnd = new SecureGenerateRandom(1)
    rnd.nextInt
  }

  def testIntRange() {
    val rnd = new SecureGenerateRandom(1)
    rnd.nextInt(0, 10)
  }
}