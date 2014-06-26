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
package com.heatonresearch.aifh.examples.capstone.model.milestone1

import java.util.HashMap
import java.util.Map

/**
 * Calculate a histogram of string values.  A count is kept for each unique value.
 */
class CalcHistogram {
  def update(key: String) {
    if (this.histogram.containsKey(key)) {
      val count: Int = this.histogram.get(key)
      this.histogram.put(key, count + 1)
    }
    else {
      this.histogram.put(key, 1)
    }
  }

  /**
   * @return The string key that has the greatest frequency.
   */
  def max: String = {
    var maxCount: Int = 0
    var result: String = null
    import scala.collection.JavaConversions._
    for (key <- this.histogram.keySet) {
      val count: Int = this.histogram.get(key)
      if ((result == null) || (maxCount < count) || (maxCount == count && result.compareTo(key) < 0)) {
        result = key
        maxCount = count
      }
    }
    result
  }

  /**
   * @return The string key with the lowest frequency.
   */
  def min: String = {
    var maxCount: Int = 0
    var result: String = null
    import scala.collection.JavaConversions._
    for (key <- this.histogram.keySet) {
      val count: Int = this.histogram.get(key)
      if ((result == null) || (maxCount > count) || (maxCount == count && result.compareTo(key) > 0)) {
        result = key
        maxCount = count
      }
    }
    result
  }

  /**
   * @return The histogram map.
   */
  def getHistogram: java.util.Map[String, Integer] = histogram

  /**
   * The counters for each unique value.
   */
  private val histogram: java.util.Map[String, Integer] = new java.util.HashMap[String, Integer]
}