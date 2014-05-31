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
package com.heatonresearch.aifh.examples.ga.iris

import com.heatonresearch.aifh.evolutionary.population.BasicPopulation
import com.heatonresearch.aifh.evolutionary.population.Population
import com.heatonresearch.aifh.evolutionary.species.BasicSpecies
import com.heatonresearch.aifh.evolutionary.train.basic.BasicEA
import com.heatonresearch.aifh.examples.util.SimpleLearn
import com.heatonresearch.aifh.genetic.crossover.Splice
import com.heatonresearch.aifh.genetic.genome.DoubleArrayGenome
import com.heatonresearch.aifh.genetic.genome.DoubleArrayGenomeFactory
import com.heatonresearch.aifh.genetic.mutate.MutatePerturb
import com.heatonresearch.aifh.genetic.species.ArraySpeciation
import com.heatonresearch.aifh.learning.RBFNetwork
import com.heatonresearch.aifh.learning.RBFNetworkGenomeCODEC
import com.heatonresearch.aifh.learning.score.ScoreFunction
import com.heatonresearch.aifh.learning.score.ScoreRegressionData
import com.heatonresearch.aifh.normalize.DataSet
import com.heatonresearch.aifh.randomize.GenerateRandom
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom
import java.io.InputStream

/**
* Learn the Iris data set with a RBF network trained by a genetic algorithm.
* This example groups genomes into species, and is more efficient than non-species.
*/
object ModelSpeciationIris {
  /**
   * Create an initial population.
   *
   * @param rnd   Random number generator.
   * @param codec The codec, the type of network to use.
   * @return The population.
   */
  def initPopulation(rnd: GenerateRandom, codec: RBFNetworkGenomeCODEC): Population = {
    val network = RBFNetwork(codec.getInputCount, codec.getRbfCount, codec.getOutputCount)
    val size = network.getLongTermMemory.length
    val result = new BasicPopulation(POPULATION_SIZE, new DoubleArrayGenomeFactory(size))
    val defaultSpecies = new BasicSpecies(result)
    result.speciesList += defaultSpecies
    for(i <- 0 until POPULATION_SIZE) {
      val genome = new DoubleArrayGenome(size)
      network.reset(rnd)
      System.arraycopy(network.getLongTermMemory, 0, genome.getData, 0, size)
      defaultSpecies.add(genome)
    }
    result.genomeFactory = new DoubleArrayGenomeFactory(size)
    result
  }

  def main(args: Array[String]) {
    val prg = new ModelSpeciationIris
    prg.process()
  }

  /**
   * The size of the population.
   */
  val POPULATION_SIZE: Int = 1000
}

class ModelSpeciationIris extends SimpleLearn {
  import ModelSpeciationIris._
  /**
   * Run the example.
   */
  def process() {
    try {
      val iStream: InputStream = this.getClass.getResourceAsStream("/iris.csv")
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
      val codec = new RBFNetworkGenomeCODEC(4, 4, 3)
      val trainingData = ds.extractSupervised(0, codec.getInputCount, codec.getRbfCount, codec.getOutputCount)
      val pop = initPopulation(rnd, codec)
      val score: ScoreFunction = new ScoreRegressionData(trainingData)
      val genetic = new BasicEA(pop, score)
      genetic.speciation = new ArraySpeciation
      genetic.codec = codec
      genetic.addOperation(0.7, new Splice(genetic,codec.size / 5))
      genetic.addOperation(0.3, new MutatePerturb(genetic,0.1))
      performIterations(genetic, 100000, 0.05, shouldMinimize = true)
      val winner = codec.decode(genetic.getBestGenome).asInstanceOf[RBFNetwork]
      SimpleLearn.queryOneOfNOld(winner, trainingData, species)
    }
    catch {
      case t: Throwable =>
        t.printStackTrace()
    }
  }
}