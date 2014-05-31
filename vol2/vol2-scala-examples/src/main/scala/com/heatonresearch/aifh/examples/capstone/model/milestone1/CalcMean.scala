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

/**
 * Calculate the mean of a series of doubles.
 */
class CalcMean {
  /**
   * Update mean for a new value.
   *
   * @param d The next value.
   */
  def update(d: Double) {
    this.sum += d
    this.count += 1
  }

  /**
   * @return The calculated mean.
   */
  def calculate: Double = sum / count

  /**
   * How many values have we encountered so far.
   */
  private var count: Int = 0
  /**
   * What is the sum of values.
   */
  private var sum: Double = .0
}