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
 * The Euclidean distance is the straight-line distance between two points.  It is calculated by taking the
 * square root of the sum of squares differences between each point in the vector.
 * <p/>
 * http://www.heatonresearch.com/wiki/Euclidean_Distance
 */
class EuclideanDistance extends CalculateDistance {

  override def calculate(position1: Vector[Double], position2: Vector[Double]): Double = {
    val sum = position1.zip(position2).map{
      case (p1,p2) =>
        val d = p1 - p2
        d*d
    }.sum
    Math.sqrt(sum)
  }
}