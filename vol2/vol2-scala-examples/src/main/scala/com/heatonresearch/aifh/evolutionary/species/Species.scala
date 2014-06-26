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
package com.heatonresearch.aifh.evolutionary.species

import com.heatonresearch.aifh.evolutionary.genome.Genome
import com.heatonresearch.aifh.evolutionary.population.Population
import scala.collection.mutable

/**
 * Defines a species.
 */
trait Species {
  /**
   * Add a genome to this species.
   *
   * @param genome The genome to add.
   */
  def add(genome: Genome)

  /**
   * Calculate this genome's share of the next population.
   *
   * @param shouldMinimize True if we see to minimize the score.
   * @param maxScore       The best score.
   * @return The share of this species, as a percent ratio.
   */
  def calculateShare(shouldMinimize: Boolean, maxScore: Double): Double

  /**
   * The age of this species.
   */
  var age : Int

  /**
   * The best score for this species.
   */
  var bestScore: Double

  /**
   * The number of generations with no improvement.
   */
  var gensNoImprovement: Int

  /**
   * The leader of this species.
   */
  var leader : Genome

  /**
   * The members of this species.
   */
  val members: mutable.ListBuffer[Genome]

  /**
   * The offspring count.
   */
  var offspringCount: Int

  /**
   * The population.
   */
  val population: Population

  /**
   * @return The offspring share for the next iteration's population.
   */
  def getOffspringShare: Double
}