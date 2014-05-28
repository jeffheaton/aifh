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

import Jama.Matrix
import Jama.QRDecomposition
import com.heatonresearch.aifh.error.ErrorCalculation
import com.heatonresearch.aifh.error.ErrorCalculationMSE
import com.heatonresearch.aifh.general.data.BasicData

/**
 * Train a Linear Regression with Least Squares.  This will only work if you use the default identity function.
 * <p/>
 * <p/>
 * Note:, if you get this error message.
 * java.lang.RuntimeException: Matrix is rank deficient.
 * It means that a linear regression cannot be fit to your data.
 *
 * Construct the trainer.
 *
 * @param algorithm The linear regression object we are training.
 * @param trainingData The training data.
 */
class TrainLeastSquares(algorithm: MultipleLinearRegression, trainingData: Vector[BasicData]) {

  /**
   * @return The R squared value.  The coefficient of determination.
   */
  def getR2: Double = 1.0 - sse / sst

  /**
   * Train.  Single iteration.
   */
  def iteration() {
    val rowCount = trainingData.size
    val inputColCount: Int = trainingData(0).input.length
    val xMatrix = new Matrix(rowCount, inputColCount + 1)
    val yMatrix = new Matrix(rowCount, 1)
    for(row <- 0 until trainingData.size) {
      val dataRow = trainingData(row)
      val colSize = dataRow.input.length
      xMatrix.set(row, 0, 1)
      for(col <- 0 until colSize) {
        xMatrix.set(row, col + 1, dataRow.input(col))
      }
      yMatrix.set(row, 0, dataRow.ideal(0))
    }
    val qr = new QRDecomposition(xMatrix)
    val beta = qr.solve(yMatrix)

    var sum: Double = 0.0
    for(i <- 0 until inputColCount) {
      sum += yMatrix.get(i, 0)
    }
    val mean: Double = sum / inputColCount
    for(i <- 0 until inputColCount) {
      val dev: Double = yMatrix.get(i, 0) - mean
      sst += dev * dev
    }
    val residuals: Matrix = xMatrix.times(beta).minus(yMatrix)
    sse = residuals.norm2 * residuals.norm2
    for(i <- 0 until algorithm.longTermMemory.length)
      algorithm.longTermMemory(i) = beta.get(i,0)

    errorCalculation.clear()
    for (dataRow <- trainingData) {
      val output = algorithm.computeRegression(dataRow.input)
      errorCalculation.updateError(output, dataRow.ideal, 1.0)
    }
    error = errorCalculation.calculate
  }

  /**
   * @return The current error.
   */
  def getError: Double = error

  /**
   * Total sum of squares.
   */
  private var sst: Double = .0
  /**
   * Sum of squares for error.
   */
  private var sse: Double = .0
  /**
   * An error calculation method.
   */
  private val errorCalculation: ErrorCalculation = new ErrorCalculationMSE
  /**
   * The last error.
   */
  private var error: Double = .0
}
