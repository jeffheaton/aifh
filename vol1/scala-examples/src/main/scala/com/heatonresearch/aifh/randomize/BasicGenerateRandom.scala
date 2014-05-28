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

import java.util.Random


object BasicGenerateRandom {

}
/**
 * A wrapper over Java's built in random number generator.
 * @param rand The underlying random number generator.
 */
class BasicGenerateRandom private (rand: Random) extends AbstractGenerateRandom {

  /**
   * Construct a random number generator with the specified seed.
   *
   * @param seed The seed.
   */
  def this(seed: Long) { this(new Random(seed)) }

  /**
   * Construct a random number generator with a time-based seed.
   */
  def this() { this(new Random()) }


  override def nextInt: Int = rand.nextInt

  override def nextDouble(): Double = rand.nextDouble

  override def nextFloat: Float = rand.nextFloat

  override def nextLong: Long = rand.nextLong

  override def nextBoolean: Boolean = rand.nextBoolean

  override def nextGaussian: Double = rand.nextGaussian
}
