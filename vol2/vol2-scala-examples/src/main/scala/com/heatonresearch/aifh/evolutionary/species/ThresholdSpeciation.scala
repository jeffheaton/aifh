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

import com.heatonresearch.aifh.AIFH
import com.heatonresearch.aifh.AIFHError
import com.heatonresearch.aifh.evolutionary.genome.Genome
import com.heatonresearch.aifh.evolutionary.population.Population
import com.heatonresearch.aifh.evolutionary.sort.SortGenomesForSpecies
import com.heatonresearch.aifh.evolutionary.sort.SpeciesComparator
import com.heatonresearch.aifh.evolutionary.train.EvolutionaryAlgorithm
import java.io.Serializable

/**
 * Speciate based on threshold. Any genomes with a compatibility score below a
 * level will be in the same species.
 */


@SerialVersionUID(1L)
abstract class ThresholdSpeciation extends Speciation with Serializable {

  /**
   * The training being used.
   */
  private var owner: EvolutionaryAlgorithm = null
  /**
   * The minimum compatibility that two genes must have to be in the same
   * species.
   */
  var compatibilityThreshold: Double = 1.0
  /**
   * The maximum number of generations allows with no improvement. After this
   * the genomes in this species are not allowed to reproduce or continue.
   * This does not apply to top species.
   */
  var numGensAllowedNoImprovement: Int = 15
  /**
   * The maximum number of species. This is just a target. If the number of
   * species goes over this number then the compatibilityThreshold is
   * increased to decrease the number of species.
   */
  var maxNumberOfSpecies: Int = 40
  /**
   * The method used to sort the genomes in the species. More desirable
   * genomes should come first for later selection.
   */
  var sortGenomes: SortGenomesForSpecies = null
  /**
   * The population.
   */
  private var population: Population = null

  /**
   * Add a genome.
   *
   * @param species The species to add to.
   * @param genome  The genome to add.
   */
  def addSpeciesMember(species: Species, genome: Genome) {
    if (owner.validationMode) {
      if (species.members.contains(genome)) {
        throw new AIFHError("Species already contains genome: " + genome.toString)
      }
    }
    if (owner.selectionComparator.compare(genome, species.leader) < 0) {
      species.bestScore = genome.adjustedScore
      species.gensNoImprovement = 0
      species.leader = genome
    }
    species.add(genome)
  }

  /**
   * Adjust the species compatibility threshold. This prevents us from having
   * too many species. Dynamically increase or decrease the
   * compatibilityThreshold.
   */
  private def adjustCompatibilityThreshold() {
    if (maxNumberOfSpecies < 1) {
      return
    }
    val thresholdIncrement: Double = 0.01
    if (population.speciesList.size > maxNumberOfSpecies) {
      compatibilityThreshold += thresholdIncrement
    }
    else if (population.speciesList.size < 2) {
      compatibilityThreshold -= thresholdIncrement
    }
  }

  /**
   * Divide up the potential offspring by the most fit species. To do this we
   * look at the total species score, vs each individual species percent
   * contribution to that score.
   *
   * @param speciesCollection The current species list.
   * @param totalSpeciesScore The total score over all species.
   */
  private def divideByFittestSpecies(speciesCollection: List[Species], totalSpeciesScore: Double) {
    val bestSpecies = findBestSpecies
    for (species <- speciesCollection) {
      var share: Int = Math.round((species.getOffspringShare / totalSpeciesScore) * owner.population.populationSize).asInstanceOf[Int]
      if ((species eq bestSpecies) && (share == 0)) {
        share = 1
      }
      if ((species.members.size == 0) || (share == 0)) {
        removeSpecies(species)
      }
      else if ((species.gensNoImprovement > numGensAllowedNoImprovement) && (species ne bestSpecies)) {
        removeSpecies(species)
      }
      else {
        species.offspringCount = share
        // sort the list
        val sorted = species.members.sorted(sortGenomes)
        species.members.clear()
        species.members ++= sorted
      }
    }
  }

  /**
   * Find the best species.
   *
   * @return The best species.
   */
  def findBestSpecies: Species = {
    if (owner.getBestGenome != null)
      owner.getBestGenome.species
    else
      null
  }

  /**
   * Attempt to remove a removable species. If the species is the best
   * species, then do not remove it. If the species is the last species, don't
   * remove it.
   *
   * @param species The species to attempt to remove.
   */
  def removeSpecies(species: Species) {
    if (species ne findBestSpecies) {
      if (population.speciesList.size > 1) {
        population.speciesList -= species
      }
    }
  }

  /**
   * If no species has a good score then divide the potential offspring amount
   * all species evenly.
   *
   * @param speciesCollection The current set of species.
   */
  private def divideEven(speciesCollection: List[Species]) {
    val ratio = 1.0 / speciesCollection.size
    for (species <- speciesCollection) {
      val share: Int = Math.round(ratio * owner.population.populationSize).asInstanceOf[Int]
      species.offspringCount = share
    }
  }

  /**
   * @return the owner
   */
  def getOwner: EvolutionaryAlgorithm = owner

  override def init(theOwner: EvolutionaryAlgorithm) {
    owner = theOwner
    population = theOwner.population
    sortGenomes = new SortGenomesForSpecies(owner)
  }

  /**
   * Level off all of the species shares so that they add up to the desired
   * population size. If they do not add up to the desired species size, this
   * was a result of rounding the floating point share amounts to integers.
   */
  private def levelOff() {
    var total = 0
    val list = population.speciesList.sorted(new SpeciesComparator(owner))
    if (list.isEmpty) {
      throw new AIFHError("Can't speciate, next generation contains no species.")
    }


    if (list(0).offspringCount == 0) {
      list(0).offspringCount = 1
    }
    import scala.collection.JavaConversions._
    for (species <- list) {
      total += species.offspringCount
    }
    var diff: Int = population.populationSize - total
    if (diff < 0) {
      var index = list.size - 1
      while ((diff != 0) && (index > 0)) {
        val species = list.get(index)
        val t: Int = Math.min(species.offspringCount, Math.abs(diff))
        species.offspringCount = species.offspringCount - t
        if (species.offspringCount == 0) {
          list.remove(index)
        }
        diff += t
        index -= 1
      }
    }
    else {
      list.get(0).offspringCount = list.get(0).offspringCount + diff
    }
  }

  override def performSpeciation(genomeList: List[Genome]) {
    import scala.collection.JavaConversions._
    val newGenomeList = resetSpecies(genomeList)
    speciateAndCalculateSpawnLevels(newGenomeList)
  }

  /**
   * Reset for an iteration.
   *
   * @return The genomes to speciate.
   */
  private def resetSpecies(inputGenomes: java.util.List[Genome]): List[Genome] = {
    val result = new java.util.ArrayList[Genome]
    val speciesArray = population.speciesList.toArray
    import scala.collection.JavaConversions._
    for (genome <- inputGenomes) {
      result.add(genome)
    }
    for (element <- speciesArray) {
      val s: BasicSpecies = element.asInstanceOf[BasicSpecies]
      s.purge()
      if (!inputGenomes.contains(s.leader)) {
        removeSpecies(s)
      }
      else if (s.gensNoImprovement > numGensAllowedNoImprovement) {
        removeSpecies(s)
      }
      result.remove(s.leader)
    }
    if (population.speciesList.isEmpty)
      throw new AIFHError("Can't speciate, the population is empty.")

    result.toList
  }

  /**
   * Determine the species.
   *
   * @param genomes The genomes to speciate.
   */
  private def speciateAndCalculateSpawnLevels(genomes: List[Genome]) {
    var maxScore = 0.0
    if (genomes.size == 0)
      throw new AIFHError("Can't speciate, the population is empty.")

    val speciesCollection = population.speciesList.toList
    if (speciesCollection.isEmpty)
      throw new AIFHError("Can't speciate, there are no species.1")


    adjustCompatibilityThreshold()

    class BreakException extends Exception
    try {
      for (genome <- genomes) {
        var currentSpecies: Species = null
        if (!genome.adjustedScore.isNaN && !genome.adjustedScore.isInfinite) {
          maxScore = Math.max(genome.adjustedScore, maxScore)
        }
        for (s <- speciesCollection) {
          val compatibility = getCompatibilityScore(genome, s.leader)
          if (compatibility <= compatibilityThreshold) {
            currentSpecies = s
            addSpeciesMember(s, genome)
            genome.species = s
            throw new BreakException
          }
        }
        if (currentSpecies == null) {
          currentSpecies = new BasicSpecies(population, genome)
          population.speciesList += currentSpecies
        }
      }
    } catch {
      case _ : BreakException =>
    }
    var totalSpeciesScore: Double = 0
    for (species <- speciesCollection) {
      totalSpeciesScore += species.calculateShare(owner.scoreFunction.shouldMinimize, maxScore)
    }
    if (speciesCollection.size == 0)
      throw new AIFHError("Can't speciate, there are no species.2")

    if (totalSpeciesScore < AIFH.DEFAULT_PRECISION) {
      divideEven(speciesCollection.toList)
    }
    else {
      divideByFittestSpecies(speciesCollection.toList, totalSpeciesScore)
    }
    levelOff()
  }

  /**
   * Determine how compatible two genomes are. More compatible genomes will be
   * placed into the same species. The lower the number, the more compatible.
   *
   * @param genome1 The first genome.
   * @param genome2 The second genome.
   * @return The compatibility level.
   */
  def getCompatibilityScore(genome1: Genome, genome2: Genome): Double
}