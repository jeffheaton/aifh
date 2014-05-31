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
package com.heatonresearch.aifh.examples.swarm.pso

import com.heatonresearch.aifh.examples.util.SimpleLearn
import com.heatonresearch.aifh.learning.{MLMethod, RBFNetwork, TrainPSO}
import com.heatonresearch.aifh.learning.score.ScoreFunction
import com.heatonresearch.aifh.learning.score.ScoreRegressionData
import com.heatonresearch.aifh.normalize.DataSet
import com.heatonresearch.aifh.randomize.GenerateRandom
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom

/**
 * Learn the Iris data set with a RBF network trained by PSO.
 */
object IrisPSOExample extends App {
  val PARTICLE_COUNT: Int = 30

  val prg = new IrisPSOExample
  prg.process()

}

class IrisPSOExample extends SimpleLearn {
  import IrisPSOExample._
  /**
   * Run the example.
   */
  def process() {
    try {
      val iStream = this.getClass.getResourceAsStream("/iris.csv")
      if (iStream == null) {
        println("Cannot access data set, make sure the resources are available.")
        System.exit(1)
      }
      val rnd: GenerateRandom = new MersenneTwisterGenerateRandom
      val ds = DataSet.load(iStream)
      ds.normalizeRange(0, -1, 1)
      ds.normalizeRange(1, -1, 1)
      ds.normalizeRange(2, -1, 1)
      ds.normalizeRange(3, -1, 1)
      val species = ds.encodeOneOfN(4)
      iStream.close()
      val particles = Array.fill(PARTICLE_COUNT) {
        val particle = RBFNetwork(4, 4, 3)
        particle.reset(rnd)
        particle
      }

      println("Particles.length = " + particles.length)
      val trainingData = ds.extractSupervised(0, 4, 4, 3)
      val score: ScoreFunction = new ScoreRegressionData(trainingData)
      val train = TrainPSO(particles.asInstanceOf[Array[MLMethod]], score)
      performIterations(train, 100000, 0.05, shouldMinimize = true)
      val winner = train.getBestParticle.asInstanceOf[RBFNetwork]
      SimpleLearn.queryOneOfNOld(winner, trainingData, species)
    }
    catch {
      case t: Throwable =>
        t.printStackTrace()
    }
  }
}