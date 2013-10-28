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
package com.heatonresearch.aifh.examples.error

import com.heatonresearch.aifh.error.ErrorCalculation
import com.heatonresearch.aifh.error.ErrorCalculationESS
import com.heatonresearch.aifh.error.ErrorCalculationMSE
import com.heatonresearch.aifh.error.ErrorCalculationRMS
import com.heatonresearch.aifh.randomize.GenerateRandom
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom
import java.text.NumberFormat

/**
 * Example that demonstrates how to calculate errors.  This allows you to see how different types of distortion affect
 * the final error for various error calculation methods.
 * <p/>
 * Type     ESS			MSE		RMS
 * Small	1252		0.01	0.1
 * Medium	31317		0.251	0.501
 * Large	125269		1.002	1.001
 * Huge	    12526940	100.216	10.011
 */
object EvaluateErrors extends App {

  /**
   * The random seed to use.
   */
  val SEED: Int = 1420
  /**
   * The number of rows.
   */
  val ROWS: Int = 10000
  /**
   * The number of columns.
   */
  val COLS: Int = 25
  /**
   * The low value.
   */
  val LOW: Double = -1
  /**
   * The high value.
   */
  val HIGH: Double = 1

  (new EvaluateErrors).process()
}

class EvaluateErrors {

  import EvaluateErrors._
  /**
   * Generate random data.
   *
   * @param seed    The seed to use.
   * @param rows    The number of rows to generate.
   * @param cols    The number of columns to generate.
   * @param low     The low value.
   * @param high    The high value.
   * @param distort The distortion factor.
   * @return The data set.
   */
  def generate(seed: Int, rows: Int, cols: Int, low: Double, high: Double, distort: Double): DataHolder = {
    val rnd: GenerateRandom = new MersenneTwisterGenerateRandom(seed)

    def generateRow = {
      val pairs = for(col <- 0 until cols) yield {
        val ideal = rnd.nextDouble(low, high)
        val actual = ideal + (rnd.nextGaussian * distort)
        (actual,ideal)
      }

      val actualRow = pairs.map(_._1).toVector
      val idealRow = pairs.map(_._2).toVector
      (actualRow,idealRow)
    }

    val rowPairs = for(row <- 0 until rows) yield generateRow
    val actual = rowPairs.map(_._1).toVector
    val ideal = rowPairs.map(_._2).toVector


    new DataHolder(actual,ideal)
  }

  /**
   * Run the example.
   */
  def process() {
    val nf = NumberFormat.getInstance
    val calcESS: ErrorCalculation = new ErrorCalculationESS
    val calcMSE: ErrorCalculation = new ErrorCalculationMSE
    val calcRMS: ErrorCalculation = new ErrorCalculationRMS
    val smallErrors: DataHolder = generate(SEED, ROWS, COLS, LOW, HIGH, 0.1)
    val mediumErrors: DataHolder = generate(SEED, ROWS, COLS, LOW, HIGH, 0.5)
    val largeErrors: DataHolder = generate(SEED, ROWS, COLS, LOW, HIGH, 1.0)
    val hugeErrors: DataHolder = generate(SEED, ROWS, COLS, LOW, HIGH, 10.0)
    val smallESS = smallErrors.calculateError(calcESS)
    val smallMSE = smallErrors.calculateError(calcMSE)
    val smallRMS = smallErrors.calculateError(calcRMS)
    val mediumESS = mediumErrors.calculateError(calcESS)
    val mediumMSE = mediumErrors.calculateError(calcMSE)
    val mediumRMS = mediumErrors.calculateError(calcRMS)
    val largeESS = largeErrors.calculateError(calcESS)
    val largeMSE = largeErrors.calculateError(calcMSE)
    val largeRMS = largeErrors.calculateError(calcRMS)
    val hugeESS = hugeErrors.calculateError(calcESS)
    val hugeMSE = hugeErrors.calculateError(calcMSE)
    val hugeRMS = hugeErrors.calculateError(calcRMS)
    println("Type\tESS\t\t\tMSE\t\tRMS")
    println("Small\t" + smallESS.toInt + "\t\t" + nf.format(smallMSE) + "\t" + nf.format(smallRMS))
    println("Medium\t" + mediumESS.toInt + "\t\t" + nf.format(mediumMSE) + "\t" + nf.format(mediumRMS))
    println("Large\t" + largeESS.toInt + "\t\t" + nf.format(largeMSE) + "\t" + nf.format(largeRMS))
    println("Huge\t" + hugeESS.toInt + "\t" + nf.format(hugeMSE) + "\t" + nf.format(hugeRMS))
  }
}