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
package com.heatonresearch.aifh.distance


/**
 * Calculate the distance between two vectors.  These vectors are double arrays.
 * Both vectors must be of the same length. These two vectors can be thought of
 * positions in a potentially high dimension space.
 * <p/>
 * You can think of a distance metric as measuring the similarity between two vectors.
 */
trait CalculateDistance {

  /**
   * Calculate the distance between two locations
   *
   * @param position1 The first vector.
   * @param position2 The second vector.
   * @return The distance.
   */
  def calculate(position1: Vector[Double], position2: Vector[Double]): Double
}
