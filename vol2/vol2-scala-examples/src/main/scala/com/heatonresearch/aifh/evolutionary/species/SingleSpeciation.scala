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

import com.heatonresearch.aifh.AIFHError
import com.heatonresearch.aifh.evolutionary.genome.Genome
import com.heatonresearch.aifh.evolutionary.sort.SortGenomesForSpecies
import com.heatonresearch.aifh.evolutionary.train.EvolutionaryAlgorithm
import java.util.Collections

/**
 * This speciation strategy simply creates a single species that contains the
 * entire population. Use this speciation strategy if you do not wish to use
 * species.
 */
class SingleSpeciation extends Speciation {

  override def init(theOwner: EvolutionaryAlgorithm) {
    owner = theOwner
    sortGenomes = new SortGenomesForSpecies(owner)
  }

  override def performSpeciation(genomeList: List[Genome]) {
    updateShare()
    val species = owner.population.speciesList(0)
    species.members.clear()
    import scala.collection.JavaConversions._
    species.members ++= genomeList
    Collections.sort(species.members, sortGenomes)
    species.leader = species.members(0)
  }

  /**
   * Update the species share of the next population.
   */
  private def updateShare() {
    val speciesCount = owner.population.speciesList.size
    if (speciesCount != 1) {
      throw new AIFHError(s"SingleSpeciation can only be used with a species count of 1, species count is $speciesCount")
    }
    val species = owner.population.speciesList(0)
    species.offspringCount = owner.population.populationSize
  }

  /**
   * The trainer.
   */
  private var owner: EvolutionaryAlgorithm = null
  /**
   * The method used to sort the genomes in the species. More desirable
   * genomes should come first for later selection.
   */
  private var sortGenomes: SortGenomesForSpecies = null
}