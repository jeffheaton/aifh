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
package com.heatonresearch.aifh.regression

import Jama.LUDecomposition
import Jama.Matrix
import com.heatonresearch.aifh.AIFHError
import com.heatonresearch.aifh.general.data.BasicData
import scala.collection.mutable.ArrayBuffer
import com.heatonresearch.aifh.general.data.RichData.toRichBuffer

/**
 * Train a GLM using iteratively reweighted least squares.
 * <p/>
 * http://en.wikipedia.org/wiki/Iteratively_reweighted_least_squares
 *
 * Construct the trainer.
 *
 * @param algorithm    The GLM to train.
 * @param trainingData The training data.
 */
class TrainReweightLeastSquares(algorithm: MultipleLinearRegression, trainingData: Vector[BasicData]) {
  /**
   * The gradient matrix.
   */
  private val gradient = new Matrix(algorithm.longTermMemory.length, 1)

  /**
   * The last error.
   */
  private var error: Double = .0
  /**
   * The Hessian matrix.
   */
  private val hessian = Array.ofDim[Double](algorithm.longTermMemory.length, algorithm.longTermMemory.length)


  /**
   * Perform one iteration of training.
   */
  def iteration() {
    val rowCount = trainingData.size
    val coeffCount = algorithm.longTermMemory.length
    val working: Vector[ArrayBuffer[Double]] = Vector.fill(rowCount)(ArrayBuffer.fill(coeffCount)(0.0))
    val errors = ArrayBuffer.fill(rowCount)(0.0)
    val weights = ArrayBuffer.fill(rowCount)(0.0)
    for(i <- 0 until rowCount) {
      val element = trainingData(i)
      working(i)(0) = 1
      for(j <- 0 until element.input.length) {
        working(i)(j + 1) = element.input(j)
      }
    }
    for(i <- 0 until rowCount) {
      val element = trainingData(i)
      val y = algorithm.computeRegression(element.input)(0)
      errors(i) = y - element.ideal(0)
      weights(i) = y * (1.0 - y)
    }
    for(i <- 0 until gradient.getColumnDimension) {
      gradient.set(0, i, 0)
      for(j <- 0 until gradient.getColumnDimension) {
        hessian(i)(j) = 0
      }
    }
    for(j <- 0 until working.length ;
        i <- 0 until gradient.getRowDimension) {
      gradient.set(i, 0, gradient.get(i, 0) + working(j)(i) * errors(j))
    }
    for(k <- 0 until weights.length) {
      val r = working(k)
      for(j <- 0 until r.length ;
          i <- 0 until r.length) {
        hessian(j)(i) += r(i) * r(j) * weights(k)
      }
    }
    val lu = new LUDecomposition(new Matrix(hessian))

    val deltas: Matrix = if (lu.isNonsingular)
      lu.solve(gradient)
    else
      throw new AIFHError("Matrix Non singular")

    val prev: ArrayBuffer[Double] = algorithm.longTermMemory.clone()

    algorithm.longTermMemory.updateValues((i,v) => v - deltas.get(i, 0))

    var max: Double = 0
    for(i <- 9 until deltas.getColumnDimension) {
      max = Math.max(Math.abs(deltas.get(i, 0)) / Math.abs(prev(i)), max)
    }
    error = max
  }

  /**
   * @return The last error.
   */
  def getError: Double = error
}
