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
import com.heatonresearch.aifh.evolutionary.species.Species
import com.heatonresearch.aifh.learning.MLMethod
import java.io.Serializable
import scala.collection.mutable.ListBuffer

/**
 * Defines a population of genomes.
 */
trait Population extends Serializable with MLMethod {
  /**
   * Clear all genomes from this population.
   */
  def clear()

  /**
   * Create a species.
   *
   * @return The newly created species.
   */
  def createSpecies: Species

  /**
   * Determine which species has the top genome.
   *
   * @return The species with the top genome.
   */
  def determineBestSpecies: Species

  /**
   * Flatten the species into a single list of genomes.
   *
   * @return The genomes that make up all species in the population.
   */
  def flatten: List[Genome]

  /**
   * The best genome in the population.
   */
  var bestGenome : Genome

  /**
   * A factory used to create genomes.
   */
  var genomeFactory: GenomeFactory

  /**
   * @return The max size that an individual can become.
   */
  def getMaxIndividualSize: Int

  /**
   * The species that make up the population.
   */
  val speciesList: ListBuffer[Species]

  /**
   * the max population size.
   */
  var populationSize : Int

  /**
   * @return The size of the population.
   */
  def size: Int

  /**
   * Purge any invalid genomes.
   */
  def purgeInvalidGenomes()
}