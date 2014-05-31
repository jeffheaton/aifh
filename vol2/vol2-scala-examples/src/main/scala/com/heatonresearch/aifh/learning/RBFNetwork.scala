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
package com.heatonresearch.aifh.learning

import com.heatonresearch.aifh.general.VectorUtil
import com.heatonresearch.aifh.general.fns.FnRBF
import com.heatonresearch.aifh.general.fns.GaussianFunction
import com.heatonresearch.aifh.randomize.GenerateRandom

object RBFNetwork {
  def apply(inputCount: Int, rbfCount: Int, outputCount: Int) : RBFNetwork = {

    val inputWeightCount = inputCount * rbfCount
    val outputWeightCount = (rbfCount + 1) * outputCount
    val rbfParams: Int = (inputCount + 1) * rbfCount
    val longTermMemory = new Array[Double](inputWeightCount + outputWeightCount + rbfParams)
    val indexInputWeights = 0
    val indexOutputWeights = inputWeightCount + rbfParams
    val rbf = new Array[FnRBF](rbfCount)
    for(i <- 0 until rbfCount) {
      val rbfIndex: Int = inputWeightCount + ((inputCount + 1) * i)
      rbf(i) = new GaussianFunction(inputCount, longTermMemory, rbfIndex)
    }

    new RBFNetwork(inputCount,outputCount,rbf,longTermMemory,indexInputWeights,indexOutputWeights)
  }
}

/**
 * A RBF network is an advanced machine learning algorithm that uses a series of RBF functions to perform
 * regression.  It can also perform classification by means of one-of-n encoding.
 * <p/>
 * The long term memory of a RBF network is made up of the widths and centers of the RBF functions, as well as
 * input and output weighting.
 * <p/>
 * http://en.wikipedia.org/wiki/RBF_network
 */
class RBFNetwork private (val inputCount: Int,
                 val outputCount: Int,
                 val rbf : Array[FnRBF],
                 val longTermMemory : Array[Double],
                 val indexInputWeights: Int,
                 val indexOutputWeights: Int) extends RegressionAlgorithm with ClassificationAlgorithm {


  override def computeRegression(input: Array[Double]): Array[Double] = {
    val rbfOutput: Array[Double] = new Array[Double](rbf.length + 1)
    rbfOutput(rbfOutput.length - 1) = 1
    for(rbfIndex <- 0 until rbf.length) {
      val weightedInput: Array[Double] = new Array[Double](input.length)
      for(inputIndex <- 0 until input.length) {
        val memoryIndex: Int = this.indexInputWeights + (rbfIndex * this.inputCount) + inputIndex
        weightedInput(inputIndex) = input(inputIndex) * this.longTermMemory(memoryIndex)
      }
      rbfOutput(rbfIndex) = this.rbf(rbfIndex).evaluate(weightedInput)
    }

    val result: Array[Double] = new Array[Double](this.outputCount)
    for(outputIndex <- 0 until result.length) {
      var sum: Double = 0
      for(rbfIndex <- 0 until rbfOutput.length) {
        val memoryIndex: Int = this.indexOutputWeights + (outputIndex * (rbf.length + 1)) + rbfIndex
        sum += rbfOutput(rbfIndex) * this.longTermMemory(memoryIndex)
      }
      result(outputIndex) = sum
    }
    result
  }

  override def getLongTermMemory: Array[Double] = longTermMemory

  /**
   * Randomize the long term memory, with the specified random number generator.
   *
   * @param rnd A random number generator.
   */
  def reset(rnd: GenerateRandom) {
    for(i <- 0 until longTermMemory.length)
      longTermMemory(i) = rnd.nextDouble(-1, 1)
  }

  override def computeClassification(input: Array[Double]): Int = {
    val output: Array[Double] = computeRegression(input)
    if (output.length > 1) {
      VectorUtil.maxIndex(output)
    }
    else {
      output(0).asInstanceOf[Int]
    }
  }

  override def toString: String = {
    "[RBFNetwork:inputCount=" + this.inputCount + ",outputCount=" + this.outputCount + ",RBFs=" +
      java.util.Arrays.toString(this.rbf.asInstanceOf[Array[AnyRef]]) + "]"
  }
}