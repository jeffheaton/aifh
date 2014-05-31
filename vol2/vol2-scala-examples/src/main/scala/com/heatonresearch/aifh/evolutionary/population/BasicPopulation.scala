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
package com.heatonresearch.aifh.evolutionary.population

import com.heatonresearch.aifh.evolutionary.genome.Genome
import com.heatonresearch.aifh.evolutionary.genome.GenomeFactory
import com.heatonresearch.aifh.evolutionary.species.BasicSpecies
import com.heatonresearch.aifh.evolutionary.species.Species
import java.io.Serializable
import scala.collection.mutable.ListBuffer

/**
 * Defines the basic functionality for a population of genomes. The population
 * is made up of species. These species contain the individiual genomes that
 * make up the population. If you do not want to use species, then create one
 * species that holds every genome.
 */
@SerialVersionUID(1L)
class BasicPopulation(thePopulationSize: Int, theGenomeFactory: GenomeFactory) extends Population with Serializable {

  def this() {
    this(0,null)
  }
  /**
   * The object name.
   */
  var name: String = null
  /**
   * The species that make up the population.
   */
  val speciesList = ListBuffer[Species]()
  /**
   * The best genome.
   */
  var bestGenome: Genome = null
  /**
   * A factory that can be used to store create genomes.
   */
  var genomeFactory: GenomeFactory = theGenomeFactory
  /**
   * How many genomes should be created.
   */
  var populationSize: Int = thePopulationSize

  override def clear() {
    speciesList.clear()
  }

  override def createSpecies: Species = {
    val species = new BasicSpecies(this)
    speciesList += species
    species
  }

  override def determineBestSpecies: Species = {
    for (species <- speciesList) {
      if (species.members.contains(bestGenome)) {
        return species
      }
    }
    null
  }

  override def flatten: List[Genome] = {
    speciesList.flatMap(_.members).toList
  }

  override def getMaxIndividualSize: Int = Integer.MAX_VALUE

  override def size: Int = flatten.size

  /**
   * Not supported, a population cannot be fit by methods that use getLongTermMemory.
   * The population is made up of many elements.
   *
   * @return Nothing.
   */
  def getLongTermMemory: Array[Double] = {
    throw new UnsupportedOperationException
  }

  /**
   * Purge any invalid genomes.
   */
  def purgeInvalidGenomes() {
    var speciesNum: Int = 0
    while (speciesNum < speciesList.size) {
      val species = speciesList(speciesNum)
      var genomeNum: Int = 0
      while (genomeNum < species.members.size) {
        val genome = species.members(genomeNum)
        if (genome.score.isInfinite || genome.adjustedScore.isInfinite ||
          genome.score.isNaN || genome.adjustedScore.isNaN) {
          species.members -= genome
        } else {
          genomeNum += 1
        }
      }
      if (species.members.size == 0) {
        speciesList -= species
      } else {
        if (!species.members.contains(species.leader)) {
          species.leader = species.members(0)
          species.bestScore = species.leader.score
        }
        speciesNum += 1
      }
    }
  }
}