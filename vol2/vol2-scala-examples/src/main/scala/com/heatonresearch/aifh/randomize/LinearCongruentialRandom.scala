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

/**
 * A Linear Congruential random number generator.  A Linear Congruential Generator (LCG) yields a sequence of
 * randomized numbers calculated with a linear equation. The method represents one of the oldest and best-known
 * pseudorandom number generator algorithms. Most programming languages use this technique.
 * <p/>
 * http://en.wikipedia.org/wiki/Linear_congruential_generator
 * Donald Knuth, The Art of Computer Programming, Volume 3, Section 3.2.1
 */
object LinearCongruentialRandom {
  /**
   * First part of default mod.
   */
  val DEFAULT_MOD1 = 2L
  /**
   * Second part of default mod.
   */
  val DEFAULT_MOD2 = 32L
  /**
   * Default mult.
   */
  val DEFAULT_MULT = 1103515245L
  /**
   * Default inc.
   */
  val DEFAULT_INC = 12345L

  /**
   * The maximum rand number that the standard GCC based LCG will generate.
   */
  val MAX_RAND = 4294967295L

}

/**
 * Create a LCG with the specified modulus, multiplier and increment. Unless
 * you REALLY KNOW WHAT YOU ARE DOING, just use the constructor that just
 * takes a seed. It will set these values to the same as set by the GCC C
 * compiler. Setting these values wrong can create fairly useless random
 * numbers.
 *
 * @param modulus    The modulus for the LCG algorithm.
 * @param multiplier The multiplier for the LCG algorithm.
 * @param increment  The increment for the LCG algorithm.
 * @param initialSeed       The seed for the LCG algorithm. Using the same seed will give
 *                      the same random number sequence each time, whether in Java or
 *                      DotNet.
 */
class LinearCongruentialRandom(val modulus: Long, val multiplier: Long, val increment: Long, initialSeed: Long) extends AbstractBoxMuller {
  import LinearCongruentialRandom._

  /**
   * Construct the default LCG. You need only specify a seed.
   *
   * @param theSeed The seed to use.
   */
  def this(theSeed: Long) {
    this(Math.pow(LinearCongruentialRandom.DEFAULT_MOD1, LinearCongruentialRandom.DEFAULT_MOD2).toLong,
      LinearCongruentialRandom.DEFAULT_MULT, LinearCongruentialRandom.DEFAULT_INC, theSeed)
  }

  /**
   * Constructor to use a seed equal to system time.
   */
  def this() {
    this(System.currentTimeMillis)
  }

  /**
   * The current seed, set to an initial value and always holds the value of
   * the last random number generated.
   */
  private var seed: Long = initialSeed % MAX_RAND

  /**
   * @return The current seed. Set to a constant to start, thereafter the
   *         previously generated random number.
   */
  final def getSeed: Long = seed

  /**
   * @return The next random number as a double between 0 and 1.
   */
  override final def nextDouble(): Double = nextLong.toDouble / LinearCongruentialRandom.MAX_RAND

  /**
   * @return The next random number as a long between 0 and MAX_RAND.
   */
  override final def nextLong: Long = {
    seed = (multiplier * seed + increment) % modulus
    seed
  }

  override def nextBoolean: Boolean = nextDouble() > 0.5

  override def nextFloat: Float = nextDouble().toFloat

  override def nextInt: Int = nextLong.toInt
}
