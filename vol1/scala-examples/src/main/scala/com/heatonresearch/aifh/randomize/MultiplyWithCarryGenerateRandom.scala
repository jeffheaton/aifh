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
 * In Multiply with Carry (MWC) is a pseudo random number generator computer science, multiply-with-carry (MWC)
 * is a method invented by George Marsaglia for generating sequences of random integers based on an initial set
 * from two to many thousands of randomly chosen seed values. The main advantages of the MWC method are that it
 * invokes simple computer integer arithmetic and leads to very fast generation of sequences of random numbers
 * with immense periods.
 * <p/>
 * <p/>
 * This class was implemented using information from the following sources:
 * <p/>
 * http://www.javaprogrammingforums.com/blogs/helloworld922/11-complimentary-multiply-carry-better-way-generate-pseudo-random-numbers.html
 * http://en.wikipedia.org/wiki/Multiply-with-carry
 */
class MultiplyWithCarryGenerateRandom private (seeds: Array[Long], carry: Long, initialR: Int, val multiplier: Long) extends AbstractBoxMuller {

  def this(seed: Long) {
    this(Array[Long](seed), seed / 2, 64, 987657110L)
  }

  def this() {
    this(Array[Long](System.currentTimeMillis), System.nanoTime, 64, 987657110L)
  }

  private var r: Int = 0
  private var c: Long = 0L
  private var n: Int = 0
  setR(initialR)

  private val seed = if (seeds == null || seeds.isEmpty) {
    Array[Long](System.currentTimeMillis)
  } else {
    Array.ofDim[Long](r)
  }
  val rnd = new LinearCongruentialRandom(seeds(0))
  this.c = (carry & 0xFFFFFFFFL) % multiplier
  for(i <- 0 until r) {
    this.seed(i) = 
      if (i < seeds.length) {
        seeds(i) & 0xFFFFFFFFL
      }
      else {
        rnd.nextInt & 0xFFFFFFFFL
      }
      
    if (this.seed(i) == 0xFFFFFFFFL) {
      this.seed(i) = 1L
    }
  }

  def nextDouble(): Double = ((next(26).toLong << 27) + next(27)) / (1L << 53).toDouble

  private def next(bits: Int): Int = {
    val t: Long = multiplier * seed(n) + c
    val d32: Long = t >>> 32
    c = d32 + (if ((t & 0xFFFFFFFFL) >= 0xFFFFFFFFL - d32) 1L else 0L)
    seed(n) = 0xFFFFFFFEL - (t & 0xFFFFFFFFL) - (c - d32 << 32) - c & 0xFFFFFFFFL
    val result: Long = seed(n)
    n = n + 1 & r - 1
    (result >>> 32 - bits).toInt
  }

  private def setR(initR: Int) {
    var theR = initR
    if (theR <= 0) {
      theR = 256
    }
    else {
      var validR: Boolean = true
      var a: Long = theR
      while (a != 1 && validR) {
        if (a % 2 != 0) {
          theR = 256
          validR = false
        }
        a >>>= 1
      }
    }
    this.r = theR
  }

  override def nextLong: Long = (next(32).toLong << 32) + next(32)

  override def nextBoolean: Boolean = nextDouble > 0.5

  override def nextFloat: Float = nextDouble().toFloat

  override def nextInt: Int = nextLong.toInt
}
