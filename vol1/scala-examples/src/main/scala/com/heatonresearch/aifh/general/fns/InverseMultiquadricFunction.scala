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

import scala.collection.mutable.ArrayBuffer

/**
 * The Inverse Multiquadric Radial Basis Function.
 * <p/>
 * http://en.wikipedia.org/wiki/Radial_basis_function
 * Construct the Inverse Multiquadric RBF. Each RBF will require space equal to (dimensions + 1) in the
 * params vector.
 *
 * @param theDimensions The number of dimensions.
 * @param theParams     A vector to hold the parameters.
 * @param theIndex      The index into the params vector.  You can store multiple RBF's in a vector.
 */
class InverseMultiquadricFunction(theDimensions: Int, theParams: ArrayBuffer[Double], theIndex: Int)
  extends AbstractRBF(theDimensions, theParams, theIndex) {

  override def evaluate(x: Vector[Double]): Double = {
    var value = 0.0
    val width = getWidth
    for(i <- 0 until getDimensions) {
      val center = getCenter(i)
      value += Math.pow(x(i) - center, 2) + (width * width)
    }

    1 / Math.sqrt(value)
  }
}