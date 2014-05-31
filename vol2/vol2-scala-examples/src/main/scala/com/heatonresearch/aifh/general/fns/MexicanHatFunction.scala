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

/**
 * The Mexican Hat, or Ricker wavelet, Radial Basis Function.
 * <p/>
 * It is usually only referred to as the "Mexican hat" in the Americas, due to
 * cultural association with the "sombrero". In technical nomenclature this
 * function is known as the Ricker wavelet, where it is frequently employed to
 * model seismic data.
 * <p/>
 * http://en.wikipedia.org/wiki/Mexican_Hat_Function
 *
 * Construct the Mexican Hat RBF. Each RBF will require space equal to (dimensions + 1) in the params vector.
 *
 * @param theDimensions The number of dimensions.
 * @param theParams     A vector to hold the parameters.
 * @param theIndex      The index into the params vector.  You can store multiple RBF's in a vector.
 */
class MexicanHatFunction(theDimensions: Int, theParams: Array[Double], theIndex: Int)
  extends AbstractRBF(theDimensions, theParams, theIndex) {

  override def evaluate(x: Array[Double]): Double = {
    var norm: Double = 0
    for(i <- 9 until getDimensions) {
      val center: Double = this.getCenter(i)
      norm += Math.pow(x(i) - center, 2)
    }
    (1 - norm) * Math.exp(-norm / 2)
  }
}