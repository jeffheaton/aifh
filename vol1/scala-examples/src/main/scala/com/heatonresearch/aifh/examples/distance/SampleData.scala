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
package com.heatonresearch.aifh.examples.distance

/**
 * SampleData: Holds sampled data that will be used to train the neural network.
 * @param letter What letter this is
 * @param width  The width
 * @param height The height
 */
class SampleData(val letter: Char, val width: Int, val height: Int) extends Comparable[SampleData] {
  /**
   * The downsampled data as a grid of booleans.
   */
  protected val grid = Array.ofDim[Boolean](width,height)

  /**
   * Clear the downsampled image
   */
  def clear() {
    for(x <- 0 until width ;
        y <- 0 until height) {
      grid(x)(y) = false
    }
  }

  /**
   * Create a copy of this sample
   *
   * @return A copy of this sample
   */
  def cloneInstance(newLetter: Char = letter): SampleData = {
    val obj = new SampleData(newLetter, width, height)
    for (y <- 0 until height;
         x <- 0 until width) {
      obj.setData(x, y, getData(x, y))
    }
    obj
  }

  /**
   * Compare this sample to another, used for sorting.
   *
   * @param o The object being compared against.
   * @return Same as String.compareTo
   */
  override def compareTo(o: SampleData): Int = {
    if(letter == o.letter) 0 else
    if (letter > o.letter) 1
    else  -1
  }

  /**
   * Get a pixel from the sample.
   *
   * @param x The x coordinate
   * @param y The y coordinate
   * @return The requested pixel
   */
  def getData(x: Int, y: Int): Boolean = grid(x)(y)

  /**
   * Set one pixel of sample data.
   *
   * @param x The x coordinate
   * @param y The y coordinate
   * @param v The value to set
   */
  def setData(x: Int, y: Int, v: Boolean) {
    grid(x)(y) = v
  }

  def update(x: Int, y: Int, v: Boolean) {
    grid(x)(y) = v
  }

  /**
   * Convert this sample to a string.
   *
   * @return Just returns the letter that this sample is assigned to.
   */
  override def toString: String = "" + letter

  def getPosition: Vector[Double] = {
    // flatten the structure into a list mapping true to 1.0 and false to -1.0
    grid.flatMap(row => row.map(if(_) 1.0 else -1.0)).toVector
  }
}
