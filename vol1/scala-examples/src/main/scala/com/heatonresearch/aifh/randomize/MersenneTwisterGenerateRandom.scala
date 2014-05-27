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
 * The Mersenne twister is a pseudo random number generator developed in 1997 by Makoto Matsumoto and
 * Takuji Nishimura that is based on a matrix linear recurrence over a finite binary field F2.
 * <p/>
 * References:
 * <p/>
 * http://www.cs.gmu.edu/~sean/research/
 * <p/>
 * http://en.wikipedia.org/wiki/Mersenne_twister
 * <p/>
 * Makato Matsumoto and Takuji Nishimura, "Mersenne Twister: A 623-Dimensionally Equidistributed Uniform
 * Pseudo-Random Number Generator", ACM Transactions on Modeling and. Computer Simulation,
 * Vol. 8, No. 1, January 1998, pp 3--30.
 */
object MersenneTwisterGenerateRandom {
  private val N: Int = 624
  private val M: Int = 397
  private val MATRIX_A: Int = 0x9908b0df
  private val UPPER_MASK: Int = 0x80000000
  private val LOWER_MASK: Int = 0x7fffffff
  private val TEMPERING_MASK_B: Int = 0x9d2c5680
  private val TEMPERING_MASK_C: Int = 0xefc60000
}

class MersenneTwisterGenerateRandom private (seedInit: Either[Long, Vector[Int]]) extends AbstractBoxMuller {

  private var stateVector: Array[Int] = null
  private var mti: Int = 0
  private var mag01: Array[Int] = null


  seedInit match {
    case Left(long) => setSeed(long)
    case Right(arr) => setSeed(arr)
  }
  import MersenneTwisterGenerateRandom._

  def this(seed: Long) {
    this(Left(seed))
  }

  def this(array: Vector[Int]) {
    this(Right(array))
  }

  def this() {
    this(System.currentTimeMillis())
  }

  private def setSeed(seed: Long) {
    stateVector = Array.ofDim[Int](N)
    mag01 = Array[Int](0x0,MATRIX_A)
    stateVector(0) = seed.toInt

    mti = 1
    while (mti < N) {
      stateVector(mti) = 1812433253 * (stateVector(mti - 1) ^ (stateVector(mti - 1) >>> 30)) + mti
      mti += 1
    }
  }

  private def setSeed(array: Vector[Int]) {
    var i = 1
    var j = 0
    setSeed(19650218)
    val kStart = Math.max(N,array.length)
    for(k <- kStart to 1 by -1) {
      stateVector(i) = (stateVector(i) ^ ((stateVector(i - 1) ^ (stateVector(i - 1) >>> 30)) * 1664525)) + array(j) + j
      i += 1
      j += 1
      if (i >= N) {
        stateVector(0) = stateVector(N - 1)
        i = 1
      }
      if (j >= array.length)
        j = 0
    }

    for(k <- N-1 to 1 by -1) {
      stateVector(i) = (stateVector(i) ^ ((stateVector(i - 1) ^ (stateVector(i - 1) >>> 30)) * 1566083941)) - i
      i += 1
      if (i >= N) {
        stateVector(0) = stateVector(N - 1)
        i = 1
      }
    }
    stateVector(0) = 0x80000000
  }

  protected def next(bits: Int): Int = {
    var y: Int = 0
    if (mti >= N) {
      var kk: Int = 0
      while (kk < N - M) {
        y = (stateVector(kk) & UPPER_MASK) | (stateVector(kk + 1) & LOWER_MASK)
        stateVector(kk) = stateVector(kk + M) ^ (y >>> 1) ^ mag01(y & 0x1)
        kk += 1
      }
      while (kk < N - 1) {
        y = (stateVector(kk) & UPPER_MASK) | (stateVector(kk + 1) & LOWER_MASK)
        stateVector(kk) = stateVector(kk + (M - N)) ^ (y >>> 1) ^ mag01(y & 0x1)
        kk += 1
      }
      y = (stateVector(N - 1) & UPPER_MASK) | (stateVector(0) & LOWER_MASK)
      stateVector(N - 1) = stateVector(M - 1) ^ (y >>> 1) ^ mag01(y & 0x1)
      mti = 0
    }
    y = stateVector(mti)
    mti += 1
    y ^= y >>> 11
    y ^= (y << 7) & TEMPERING_MASK_B
    y ^= (y << 15) & TEMPERING_MASK_C
    y ^= (y >>> 18)
    y >>> (32 - bits)
  }

  override def nextDouble(): Double = ((next(26).toLong << 27) + next(27)) / (1L << 53).toDouble

  override def nextLong: Long = (next(32).toLong << 32) + next(32)

  override def nextBoolean: Boolean = nextDouble > 0.5

  override def nextFloat: Float = nextDouble().toFloat

  override def nextInt: Int = nextLong.toInt
}
