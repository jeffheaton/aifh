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
package com.heatonresearch.aifh.evolutionary.opp.selection

import com.heatonresearch.aifh.evolutionary.genome.Genome
import com.heatonresearch.aifh.evolutionary.species.Species
import com.heatonresearch.aifh.evolutionary.train.EvolutionaryAlgorithm
import com.heatonresearch.aifh.evolutionary.train.basic.BasicEA
import com.heatonresearch.aifh.randomize.GenerateRandom
import java.io.Serializable

/**
 * Tournament select can be used to select a fit (or unfit) genome from a
 * species. The selection is run a set number of rounds. Each round two random
 * participants are chosen. The more fit participant continues to the next
 * round.
 * <p/>
 * http://en.wikipedia.org/wiki/Tournament_selection
 *
 * @param trainer The trainer.
 * @param rounds  The number of rounds to use.
 */
@SerialVersionUID(1L)
class TournamentSelection(var trainer: EvolutionaryAlgorithm,
                          var rounds: Int) extends SelectionOperator with Serializable {

  override def performAntiSelection(rnd: GenerateRandom, species: Species): Int = {
    var worstIndex = rnd.nextInt(species.members.size)
    var worst = species.members(worstIndex)
    BasicEA.calculateScoreAdjustment(worst, trainer.scoreAdjusters)
    for(i <- 0 until rounds) {
      val competitorIndex = rnd.nextInt(species.members.size)
      val competitor = species.members(competitorIndex)
      if (competitor.adjustedScore.isInfinite || competitor.adjustedScore.isNaN) {
        return competitorIndex
      }
      BasicEA.calculateScoreAdjustment(competitor, trainer.scoreAdjusters)
      if (!trainer.selectionComparator.isBetterThan(competitor, worst)) {
        worst = competitor
        worstIndex = competitorIndex
      }
    }
    worstIndex
  }

  override def performSelection(rnd: GenerateRandom, species: Species): Int = {
    var bestIndex = rnd.nextInt(species.members.size)
    var best = species.members(bestIndex)
    BasicEA.calculateScoreAdjustment(best, trainer.scoreAdjusters)
    for(i <- 0 until rounds) {
      val competitorIndex: Int = rnd.nextInt(species.members.size)
      val competitor = species.members(competitorIndex)
      if (!competitor.adjustedScore.isInfinite && !competitor.adjustedScore.isNaN) {
        BasicEA.calculateScoreAdjustment(competitor, trainer.scoreAdjusters)
        if (trainer.selectionComparator.isBetterThan(competitor, best)) {
          best = competitor
          bestIndex = competitorIndex
        }
      }
    }
    bestIndex
  }
}