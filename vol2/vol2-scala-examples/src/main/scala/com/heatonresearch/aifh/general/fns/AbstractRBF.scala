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
package com.heatonresearch.aifh.general.fns

import java.text.NumberFormat

/**
 * Provides the basics for an RBF function.  RBF functions take their "parameters" from a vector (and starting index).
 * This allows many RBF's to be "stacked" together in a single vector.  RBF parameters are: a single width and a
 * vector of centers.  Therefor the size required to store one RBF is (dimensions + 1).  There is no peak parameter,
 * the peak is assumed to be 1.
 *
 * Construct the RBF. Each RBF will require space equal to (dimensions + 1) in the params vector.
 *
 * @param dimensions The number of dimensions.
 * @param params     A vector to hold the paramaters.
 * @param index      The index into the params vector.  You can store multiple RBF's in a vector.
 */
abstract class AbstractRBF(val dimensions: Int,val params: Array[Double], val index: Int) extends FnRBF {


  /**
   * The index to the widths.
   */
  private val indexWidth: Int = index
  /**
   * The index to the centers.
   */
  private final val indexCenters: Int = index + 1

  override final def getCenter(dimension: Int): Double = params(indexCenters + dimension)

  override final def getDimensions: Int = dimensions

  override final def getWidth: Double = params(indexWidth)

  override final def setWidth(theWidth: Double) {
    this.params(indexWidth) = theWidth
  }

  override def toString: String = {
    val f: NumberFormat = NumberFormat.getNumberInstance
    f.setMinimumFractionDigits(2)
    val result: StringBuilder = new StringBuilder
    result.append("[")
    result.append(this.getClass.getSimpleName)
    result.append(":width=")
    result.append(f.format(this.getWidth))
    result.append(",center=")
    for(i <- 0 until dimensions) {
      if (i > 0) {
        result.append(",")
      }
      result.append(f.format(this.params(this.indexCenters + i)))
    }
    result.append("]")
    result.toString()
  }

  override def setCenter(dimension: Int, value: Double) {
    this.params(indexCenters + dimension) = value
  }
}