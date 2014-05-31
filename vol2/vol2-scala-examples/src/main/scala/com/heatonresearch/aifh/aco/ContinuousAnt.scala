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
package com.heatonresearch.aifh.aco

/**
 * An individual ant for continuous ACO.
 * @param n                 The number of parameters (dimensions).
 * @param shouldMinimize True, if this ant should minimize.  This value should be the same for all ants.
 */
class ContinuousAnt(val n : Int,val shouldMinimize : Boolean) extends Comparable[ContinuousAnt] {

  /**
   * The score for this ant.
   */
  var score: Double = 0.0
  /**
   * The parameters for this ant.
   */
  val params = new Array[Double](n)

  override def compareTo(o: ContinuousAnt): Int = {
    if (shouldMinimize)
      score.compare(o.score)
    else
      o.score.compare(score)
  }
}