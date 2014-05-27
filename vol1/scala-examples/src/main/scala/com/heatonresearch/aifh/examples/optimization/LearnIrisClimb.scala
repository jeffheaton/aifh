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
package com.heatonresearch.aifh.examples.optimization

import com.heatonresearch.aifh.examples.learning.SimpleLearn
import com.heatonresearch.aifh.learning.RBFNetwork
import com.heatonresearch.aifh.learning.TrainHillClimb
import com.heatonresearch.aifh.learning.score.ScoreFunction
import com.heatonresearch.aifh.learning.score.ScoreRegressionData
import com.heatonresearch.aifh.normalize.DataSet
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom

/**
* Use a RBF network to learn the Iris data set, trained by hill climbing.
*/
object LearnIrisClimb extends App {
  (new LearnIrisClimb).process()
}

class LearnIrisClimb extends SimpleLearn {
  /**
   * Run the example.
   */
  def process() {
    try {
      val iStream = this.getClass.getResourceAsStream("/iris.csv")
      val ds = DataSet.load(iStream)
      ds.normalizeRange(0, 0, 1)
      ds.normalizeRange(1, 0, 1)
      ds.normalizeRange(2, 0, 1)
      ds.normalizeRange(3, 0, 1)
      val species = ds.encodeEquilateral(4)
      iStream.close()
      val trainingData = ds.extractSupervised(0, 4, 4, 2)
      val network = new RBFNetwork(4, 4, 2)
      network.reset(new MersenneTwisterGenerateRandom())
      val scoreFn = new ScoreRegressionData(trainingData)
      val train = new TrainHillClimb(true, network, scoreFn)
      performIterations(train, 100000, 0.01, shouldMinimize = true)
      SimpleLearn.queryEquilateral(network, trainingData, species, 0, 1)
    }
    catch {
      case t: Throwable =>
        t.printStackTrace()
    }
  }
}
