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
package com.heatonresearch.aifh.examples.learning

import com.heatonresearch.aifh.learning.RBFNetwork
import com.heatonresearch.aifh.learning.TrainGreedyRandom
import com.heatonresearch.aifh.learning.score.ScoreFunction
import com.heatonresearch.aifh.learning.score.ScoreRegressionData
import com.heatonresearch.aifh.normalize.DataSet
import java.io.InputStream

/**
* Learn the Iris data using an RBF network taught by the Greedy Random algorithm.
*/
object LearnIris extends App {
  (new LearnIris).process()
}

class LearnIris extends SimpleLearn {
  /**
   * Run the example.
   */
  def process() {
    try {
      val istream: InputStream = this.getClass.getResourceAsStream("/iris.csv")
      val ds: DataSet = DataSet.load(istream)
      istream.close()
      // The following ranges are setup for the Iris data set.  If you wish to normalize other files you will
      // need to modify the below function calls other files.
      ds.normalizeRange(0, 0, 1)
      ds.normalizeRange(1, 0, 1)
      ds.normalizeRange(2, 0, 1)
      ds.normalizeRange(3, 0, 1)
      val species = ds.encodeEquilateral(4)
      val trainingData = ds.extractSupervised(0, 4, 4, 2)
      val network = new RBFNetwork(4, 4, 2)
      val score: ScoreFunction = new ScoreRegressionData(trainingData)
      val train = new TrainGreedyRandom(true, network, score)
      performIterations(train, 100000, 0.01, shouldMinimize = true)
      SimpleLearn.queryEquilateral(network, trainingData, species, 0, 1)
    }
    catch {
      case t: Throwable =>
        t.printStackTrace()
    }
  }
}