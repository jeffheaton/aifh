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
import com.heatonresearch.aifh.general.fns.link.LogitLinkFunction
import com.heatonresearch.aifh.normalize.DataSet
import com.heatonresearch.aifh.regression.MultipleLinearRegression
import com.heatonresearch.aifh.regression.TrainReweightLeastSquares
import java.io.InputStream

/**
 * Example that uses a GLM to predict the probability of breast cancer.
 */
object GLMExample extends App {
  (new GLMExample).process()
}

class GLMExample extends SimpleLearn {
  /**
   * Run the example.
   */
  def process() {
    try {
      val istream: InputStream = this.getClass.getResourceAsStream("/breast-cancer-wisconsin.csv")
      val ds = DataSet.load(istream)
      istream.close()
      ds.deleteUnknowns()
      ds.deleteColumn(0)
      ds.replaceColumn(9, 4, 1, 0)
      val trainingData = ds.extractSupervised(0, 9, 9, 1)
      val reg = new MultipleLinearRegression(9)
      reg.linkFunction = new LogitLinkFunction
      val train = new TrainReweightLeastSquares(reg, trainingData)
      var iteration: Int = 0
      do {
        iteration += 1
        train.iteration()
        println(s"Iteration #$iteration, Error: ${train.getError}")
      } while (iteration < 1000 && train.getError > 0.01)
      SimpleLearn.query(reg, trainingData)
      println(s"Error: ${train.getError}")
    }
    catch {
      case t: Throwable =>
        t.printStackTrace()
    }
  }
}