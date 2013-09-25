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
package com.heatonresearch.aifh.examples.randomize

import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom

/**
 * Approximate PI by Monte Carlo.
 * <p/>
 * http://en.wikipedia.org/wiki/Monte_Carlo_method
 */
object EvaluatePI extends App {
  (new EvaluatePI).process()
}

class EvaluatePI {
  /**
   * Random number generator.
   */
  private val rnd = new MersenneTwisterGenerateRandom()

  def process() {
    var tries: Long = 0
    var success: Int = 0
    var lastUpdate: Int = 0
    var x: Double = .0
    var y: Double = .0
    (0 until 1000000000) foreach { i =>
      x = rnd.nextDouble()
      y = rnd.nextDouble()
      tries += 1
      if (x * x + y * y <= 1)
        success += 1
      lastUpdate += 1
      if (lastUpdate >= 1000000) {
        val pi: Double = 4 * success.asInstanceOf[Double] / tries.asInstanceOf[Double]
        println(s"Tries=$tries, pi=$pi")
        lastUpdate = 0
      }
    }
  }

}