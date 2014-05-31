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
import java.io.Serializable
import scala.collection.mutable

/**
 * Provides basic functionality for a species.
 * @param thePopulation The population the species belongs to.
 */
@SerialVersionUID(1L)
class BasicSpecies(thePopulation: Population) extends Species with Serializable {

  /**
   * The age of this species.
   */
  override var age: Int = 0
  /**
   * The best score.
   */
  override var bestScore: Double = 0.0
  /**
   * The number of generations with no improvement.
   */
  override var gensNoImprovement: Int = 0
  /**
   * The leader.
   */
  override var leader: Genome = null
  /**
   * The list of genomes.
   */
  override val members = mutable.ListBuffer[Genome]()
  /**
   * The owner class.
   */
  override val population: Population = thePopulation
  /**
   * The offspring count.
   */
  @transient
  override var offspringCount: Int = 0
  /**
   * The offpsring share (percent).
   */
  @transient
  private var offspringShare: Double = .0

  /**
   * Construct a species.
   *
   * @param theFirst      The first genome in the species.
   */
  def this(thePopulation: Population, theFirst: Genome) {
    this(thePopulation)
    bestScore = theFirst.score
    gensNoImprovement = 0
    age = 0
    leader = theFirst
    members += theFirst
  }

  override def add(genome: Genome) {
    genome.population = population
    members += genome
  }

  override def calculateShare(shouldMinimize: Boolean, maxScore: Double): Double = {
    var total = 0.0
    var count = 0
    for (genome <- members) {
      if (!genome.adjustedScore.isNaN && !genome.adjustedScore.isInfinite) {
        var s = 0.0
        if (shouldMinimize)
          s = maxScore - genome.adjustedScore
        else
          s = genome.adjustedScore
        total += s
        count += 1
      }
    }
    if (count == 0)
      offspringShare = 0
    else
      offspringShare = total / count

    offspringShare
  }

  override def getOffspringShare: Double = offspringShare

  /**
   * Purge all members, increase age by one and count the number of
   * generations with no improvement.
   */
  def purge() {
    members.clear()
    if (leader != null) {
      members += leader
    }
    age += 1
    gensNoImprovement += 1
    offspringCount = 0
    offspringShare = 0
  }

  override def toString: String = {
    s"[BasicSpecies: score=$bestScore, members=${members.size}, age=$age, " +
      s"no_improv=$gensNoImprovement, share=$offspringShare, offspring count=$offspringShare]"
  }
}