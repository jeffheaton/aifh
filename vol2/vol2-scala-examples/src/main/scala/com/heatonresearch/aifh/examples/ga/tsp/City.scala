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
package com.heatonresearch.aifh.examples.ga.tsp

/**
 * City: Holds the location of a city for the traveling salesman problem.
 *
 * @param xpos The city's x position
 * @param ypos The city's y position.
 * @author Jeff Heaton
 */
class City(val xpos : Int,val ypos: Int) {

  /**
   * Return's the city's x position.
   *
   * @return The city's x position.
   */
  private[tsp] def getx: Int = xpos

  /**
   * Returns the city's y position.
   *
   * @return The city's y position.
   */
  private[tsp] def gety: Int = ypos

  /**
   * Returns how close the city is to another city.
   *
   * @param cother The other city.
   * @return A distance.
   */
  def proximity(cother: City): Int = proximity(cother.getx, cother.gety)

  /**
   * Returns how far this city is from a a specific point. This method uses
   * the pythagorean theorem to calculate the distance.
   *
   * @param x The x coordinate
   * @param y The y coordinate
   * @return The distance.
   */
  private[tsp] def proximity(x: Int, y: Int): Int = {
    val xdiff: Int = this.xpos - x
    val ydiff: Int = this.ypos - y
    Math.sqrt(xdiff * xdiff + ydiff * ydiff).asInstanceOf[Int]
  }
}