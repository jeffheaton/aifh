/*
 * Artificial Intelligence for Humans
 * Volume 2: Nature Inspired Algorithms
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh
 *
 * Copyright 2014 by Jeff Heaton
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

import com.heatonresearch.aifh.AIFHError
import java.io.Serializable

/**
 * Generate random choices unevenly.  This class is used to select random
 * choices from a list, with a probability weight places on each item
 * in the list.
 * <p/>
 * This is often called a Roulette Wheel in Machine Learning texts.  How it differs from
 * a Roulette Wheel that you might find in Las Vegas or Monte Carlo is that the
 * areas that can be selected are not of uniform size.  However, you can be sure
 * that one will be picked.
 * <p/>
 * http://en.wikipedia.org/wiki/Fitness_proportionate_selection
 *
 * @param theProbabilities The probability of each item in the list.
 */
class RandomChoice(theProbabilities: Array[Double]) extends Serializable {


  /**
   * The probabilities of each item in the list.
   */
  private val probabilities: Array[Double] = theProbabilities.clone()

  {
    var total: Double = 0
    for (probability <- probabilities) {
      total += probability
    }
    if (total == 0.0) {
      val prob: Double = 1.0 / probabilities.length
      for(i <- 0 until probabilities.length)
        probabilities(i) = prob
    }
    else {
      var total2: Double = 0
      val factor: Double = 1.0 / total
      for(i <- 0 until probabilities.length) {
        probabilities(i) = probabilities(i) * factor
        total2 += probabilities(i)
      }
      if (Math.abs(1.0 - total2) > 0.02) {
        val prob: Double = 1.0 / probabilities.length
        for(i <- 0 until probabilities.length) {
          probabilities(i) = prob
        }
      }
    }
  }

  /**
   * Generate a random choice, based on the probabilities provided to the constructor.
   *
   * @return The random choice.
   */
  def generate(theGenerator: GenerateRandom): Int = {
    val r = theGenerator.nextDouble()
    var sum = 0.0
    for(i <- 0 until probabilities.length) {
      sum += probabilities(i)
      if (r < sum) {
        return i
      }
    }
    for(i <- 0 until probabilities.length) {
      if (probabilities(i) != 0.0) {
        return i
      }
    }
    throw new AIFHError("Invalid probabilities.")
  }

}