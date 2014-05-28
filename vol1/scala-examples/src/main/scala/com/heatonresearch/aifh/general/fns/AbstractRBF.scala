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
package com.heatonresearch.aifh.general.fns

import java.text.NumberFormat
import scala.collection.mutable.ArrayBuffer

/**
 * Provides the basics for an RBF function.  RBF functions take their "parameters" from a vector (and starting index).
 * This allows many RBF's to be "stacked" together in a single vector.  RBF parameters are: a single width and a
 * vector of centers.  Therefor the size required to store one RBF is (dimensions + 1).  There is no peak parameter,
 * the peak is assumed to be 1.
 *
 * Construct the RBF. Each RBF will require space equal to (dimensions + 1) in the params vector.
 *
 * @param dimensions The number of dimensions.
 * @param params     The parameter vector.  Holds the RBF width and centers.  This vector may hold multiple RBF's.
 * @param theIndex      The index into the params vector.  You can store multiple RBF's in a vector.

 */
abstract class AbstractRBF(dimensions: Int, params: ArrayBuffer[Double], theIndex: Int) extends FnRBF {

  /**
   * The index to the widths.
   */
  private val indexWidth = theIndex
  /**
   * The index to the centers.
   */
  private val indexCenters: Int = theIndex + 1

  final override def getCenter(dimension: Int): Double = {
    params(indexCenters + dimension)
  }

  final override def getDimensions: Int = dimensions

  final override def getWidth: Double = params(indexWidth)

  override def setWidth(theWidth: Double) {
    params(indexWidth) = theWidth
  }

  override def toString: String = {
    val f = NumberFormat.getNumberInstance
    f.setMinimumFractionDigits(2)
    val centersStr = (indexCenters until (indexCenters+dimensions)).map(params(_)).map(f.format).mkString(",")
    s"[${getClass.getSimpleName}:width=${f.format(this.getWidth)},center=$centersStr]"
  }

  override def setCenter(dimension: Int, value: Double) {
    params(indexCenters + dimension) = value
  }
}
