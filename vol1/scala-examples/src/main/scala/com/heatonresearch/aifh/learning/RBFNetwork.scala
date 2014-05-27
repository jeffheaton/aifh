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
package com.heatonresearch.aifh.learning

import com.heatonresearch.aifh.general.fns.FnRBF
import com.heatonresearch.aifh.general.fns.GaussianFunction
import com.heatonresearch.aifh.randomize.GenerateRandom
import com.heatonresearch.aifh.general.data.RichData
import RichData._
import scala.collection.mutable.ArrayBuffer

/**
 * A RBF network is an advanced machine learning algorithm that uses a series of RBF functions to perform
 * regression.  It can also perform classification by means of one-of-n encoding.
 * <p/>
 * The long term memory of a RBF network is made up of the widths and centers of the RBF functions, as well as
 * input and output weighting.
 * <p/>
 * http://en.wikipedia.org/wiki/RBF_network
 *
 * Construct the RBF network.
 *
 * @param inputCount  The input count.
 * @param rbfCount       The number of RBF functions.
 * @param outputCount The output count.
 */
class RBFNetwork(val inputCount: Int, rbfCount: Int, val outputCount: Int) extends RegressionAlgorithm with ClassificationAlgorithm {

  /**
   * An index to the input weights in the long term memory.
   */
  private val indexInputWeights = 0

  val inputWeightCount = inputCount * rbfCount
  val outputWeightCount = (rbfCount + 1) * outputCount
  val rbfParams = (inputCount + 1) * rbfCount
  /**
   * An index to the output weights in the long term memory.
   */
  private val indexOutputWeights: Int = inputWeightCount + rbfParams

  /**
   * The weights & RBF parameters.  See constructor for layout.
   */
  override val longTermMemory = ArrayBuffer.fill(inputWeightCount + outputWeightCount + rbfParams)(0.0)

  /**
   * The RBF functions.
   */
  private val rbf: Vector[FnRBF] = {
    val arr = Array.ofDim[FnRBF](rbfCount)
    for(i <- 0 until rbfCount) {
      val rbfIndex: Int = inputWeightCount + ((inputCount + 1) * i)
      arr(i) = new GaussianFunction(inputCount, longTermMemory, rbfIndex)
    }
    arr.toVector
  }

  override def computeRegression(input: Vector[Double]): Vector[Double] = {
    val rbfOutput = ArrayBuffer.fill(rbf.length + 1)(0.0)
    rbfOutput(rbfOutput.length - 1) = 1.0
    for(rbfIndex <- 0 until rbf.length) {
      val weightedInput = for(inputIndex <- 0 until input.length) yield {
        val memoryIndex = indexInputWeights + (rbfIndex * inputCount) + inputIndex
        input(inputIndex) * longTermMemory(memoryIndex)
      }
      rbfOutput(rbfIndex) = rbf(rbfIndex).evaluate(weightedInput.toVector)

    }

    val result = ArrayBuffer.fill(outputCount)(0.0)
    for(outputIndex <- 0 until result.length) {
      var sum = 0.0
      for(rbfIndex <- 0 until rbfOutput.length) {
        val memoryIndex = indexOutputWeights + (outputIndex * (rbf.length + 1)) + rbfIndex
        sum += rbfOutput(rbfIndex) * longTermMemory(memoryIndex)
      }
      result(outputIndex) = sum
    }

    result.toVector
  }

  /**
   * Randomize the long term memory, with the specified random number generator.
   *
   * @param rnd A random number generator.
   */
  def reset(rnd: GenerateRandom) {
    for(i <- 0 until longTermMemory.length)
    longTermMemory(i) = rnd.nextDouble(-1, 1)
  }

  override def computeClassification(input: Vector[Double]): Int = {
    val output = computeRegression(input)
    output.maxIndex
  }

  override def toString: String = {
    s"[RBFNetwork:inputCount=$inputCount,outputCount=$outputCount,RBFs=${rbf.mkString("[",",","]")}]"
  }
}
