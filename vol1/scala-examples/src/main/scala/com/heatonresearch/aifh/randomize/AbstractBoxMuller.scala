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
 * Provides the ability for subclasses to generate normally distributed random numbers.
 */
object AbstractBoxMuller {
  /**
   * The mean.
   */
  val MU = 0.0
  /**
   * The standard deviation.
   */
  val SIGMA = 1.0
}

abstract class AbstractBoxMuller extends AbstractGenerateRandom {

  override def nextGaussian: Double = {
    var x1: Double = .0
    var x2: Double = .0
    var w: Double = .0
    var y1: Double = .0

    if (this.useLast) {
      y1 = this.y2
      this.useLast = false
    }
    else {
      do {
        x1 = 2.0 * nextDouble() - 1.0
        x2 = 2.0 * nextDouble() - 1.0
        w = x1 * x1 + x2 * x2
      } while (w >= 1.0)
      w = Math.sqrt((-2.0 * Math.log(w)) / w)
      y1 = x1 * w
      this.y2 = x2 * w
      this.useLast = true
    }

    AbstractBoxMuller.MU + y1 * AbstractBoxMuller.SIGMA
  }

  /**
   * The y2 value.
   */
  private var y2: Double = .0
  /**
   * Should we use the last value.
   */
  private var useLast: Boolean = false
}