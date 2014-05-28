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
package com.heatonresearch.aifh.general.data

/**
 * This class is used to store both the input and ideal vectors for a single item of training data.  A label can also
 * be applied.
 */
object BasicData {
  /**
   * Convert two 2D arrays into a List of BasicData elements.  One array holds input and the other ideal vectors.
   *
   * @param inputData An array of input vectors.
   * @param idealData An array of ideal vectors.
   * @return A list of BasicData elements.
   */
  def convertArrays(inputData: Vector[Vector[Double]], idealData: Vector[Vector[Double]]): Vector[BasicData] = {
    inputData.zip(idealData).map(p => new BasicData(p._1,p._2,null))
  }
}

/**
 * Construct a supervised element, with a label.
 *
 * @param input The input data vector.
 * @param ideal The ideal data vector.
 * @param label     The label.
 */
class BasicData(val input: Vector[Double], val ideal: Vector[Double], val label: String = null) {

  /**
   * Construct an unsupervised element, with a label.
   *
   * @param theInputData The input vector.
   * @param theLabel     The label.
   */
  def this(theInputData: Vector[Double], theLabel: String) {
    this(theInputData,Vector[Double](), theLabel)
  }

  /**
   * Construct an unsupervised element, without a label.
   *
   * @param theInputData The input vector.
   */
  def this(theInputData: Vector[Double]) {
    this(theInputData, null)
  }

  def this(theInputDimensions: Int, theIdealDimensions: Int, theLabel: String) {
    this(Vector.fill(theInputDimensions)(0.0), Vector.fill(theIdealDimensions)(0.0), theLabel)
  }

  /**
   * Construct an empty unsupervised element.  An unsupervised element does not have an expected output.
   *
   * @param theInputDimensions The number of dimensions.
   */
  def this(theInputDimensions: Int) {
    this(theInputDimensions, 0, null)
  }

  /**
   * Construct an empty supervised element.  A supervised element has both input an ideal.
   *
   * @param theInputDimensions The dimensions for the input vector.
   * @param theIdealDimensions The dimensions for the ideal vector.
   */
  def this(theInputDimensions: Int, theIdealDimensions: Int) {
    this(theInputDimensions, theIdealDimensions, null)
  }

  override def toString: String = s"[BasicData: input:$input, ideal:$ideal, label:$label]"

}
