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
 * Provides a foundation for most random number generation.  This allows the nextDouble to generate
 * the other types.
 */
abstract class AbstractGenerateRandom extends GenerateRandom {
  override def nextInt(low: Int, high: Int): Int = low + (nextDouble() * (high - low)).toInt

  override def nextDouble(high: Double): Double = nextDouble(0, high)

  override def nextDouble(low: Double, high: Double): Double = low + (nextDouble() * (high - low))

  override def nextInt(range: Int): Int = nextInt(0, range)
}