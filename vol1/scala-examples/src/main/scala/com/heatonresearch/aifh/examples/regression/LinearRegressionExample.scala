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
package com.heatonresearch.aifh.examples.regression

import com.heatonresearch.aifh.examples.learning.SimpleLearn
import com.heatonresearch.aifh.normalize.DataSet
import com.heatonresearch.aifh.regression.MultipleLinearRegression
import com.heatonresearch.aifh.regression.TrainLeastSquares
import java.io.InputStream

/**
 * Linear regression example.
 */
object LinearRegressionExample extends App {
  (new LinearRegressionExample).process()
}

class LinearRegressionExample extends SimpleLearn {
  def process() {
    try {
      val iStream: InputStream = this.getClass.getResourceAsStream("/abalone.csv")
      val ds = DataSet.load(iStream)
      ds.encodeOneOfN(0, 0, 1)
      iStream.close()
      val trainingData = ds.extractSupervised(0, 10, 10, 1)
      val reg = new MultipleLinearRegression(10)
      val train = new TrainLeastSquares(reg, trainingData)
      train.iteration()
      SimpleLearn.query(reg, trainingData)
      println(s"Error: ${train.getError}")
    } catch {
      case t: Throwable =>
        t.printStackTrace()
    }
  }
}