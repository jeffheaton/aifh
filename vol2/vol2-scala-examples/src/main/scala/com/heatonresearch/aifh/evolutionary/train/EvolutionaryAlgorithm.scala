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
package com.heatonresearch.aifh.evolutionary.train

import com.heatonresearch.aifh.evolutionary.codec.GeneticCODEC
import com.heatonresearch.aifh.evolutionary.genome.Genome
import com.heatonresearch.aifh.evolutionary.opp.EvolutionaryOperator
import com.heatonresearch.aifh.evolutionary.opp.OperationList
import com.heatonresearch.aifh.evolutionary.opp.selection.SelectionOperator
import com.heatonresearch.aifh.evolutionary.population.Population
import com.heatonresearch.aifh.evolutionary.score.AdjustScore
import com.heatonresearch.aifh.evolutionary.sort.GenomeComparator
import com.heatonresearch.aifh.evolutionary.species.Speciation
import com.heatonresearch.aifh.learning.score.ScoreFunction

/**
 * This interface defines the basic functionality of an Evolutionary Algorithm.
 * An evolutionary algorithm is one that applies operations to a population of
 * potential "solutions".
 */
trait EvolutionaryAlgorithm {
  /**
   * Add an operation.
   *
   * @param probability The probability of using this operator.
   * @param opp         The operator to add.
   */
  def addOperation(probability: Double, opp: EvolutionaryOperator)

  /**
   * Add a score adjuster. Score adjusters are used to adjust the adjusted
   * score of a genome. This allows bonuses and penalties to be applied for
   * desirable or undesirable traits.
   *
   * @param scoreAdjust The score adjustor to add.
   */
  def addScoreAdjuster(scoreAdjust: AdjustScore)

  /**
   * Calculate the score for a genome.
   *
   * @param g The genome to calculate the score for.
   */
  def calculateScore(g: Genome)

  /**
   * Called when training is finished. This allows the EA to properly shut
   * down.
   */
  def finishTraining()

  /**
   * Get the comparator that is used to choose the "true best" genome. This
   * uses the real score, and not the adjusted score.
   */
  var bestComparator: GenomeComparator

  /**
   * @return The current best genome. This genome is safe to use while the EA
   *         is running. Genomes are not modified. They simply produce
   *         "offspring".
   */
  def getBestGenome: Genome

  /**
   * The CODEC that is used to transform between genome and phenome.
   */
  var codec: GeneticCODEC

  /**
   * @return The current score. This value should either be minimized or
   *         maximized, depending on the score function.
   */
  def getLastError: Double

  /**
   * @return The current iteration number. Also sometimes referred to as
   *         generation or epoch.
   */
  def getIteration: Int

  /**
   * @return The maximum size an individual genome can be. This is an
   *         arbitrary number defined by the genome. Lower numbers are less
   *         complex.
   */
  def getMaxIndividualSize: Int

  /**
   * The maximum number to try certain genetic operations. This prevents endless loops.
   */
  var maxTries: Int

  /**
   * @return The operators.
   */
  def getOperators: OperationList

  /**
   * The population.
   */
  var population: Population

  /**
   * The score adjusters. This allows bonuses and penalties to be
   *         applied for desirable or undesirable traits.
   */
  def scoreAdjusters: List[AdjustScore]

  /**
   * The score function.
   */
  val scoreFunction : ScoreFunction

  /**
   * The selection operator. Used to choose genomes.
   */
  val selection: SelectionOperator

  /**
   * True if exceptions that occur during genetic operations should be ignored.
   */
  var shouldIgnoreExceptions: Boolean

  /**
   * The speciation method.
   */
  var speciation: Speciation

  /**
   * True if any genome validators should be applied. This takes more time but
   * can help isolate a problem.
   */
  var validationMode : Boolean

  /**
   * Perform a training iteration. Also called generations or epochs.
   */
  def iteration()

  /**
   * The comparator that is used to choose the "best" genome for
   * selection, as opposed to the "true best". This uses the adjusted score,
   * and not the score.
   */
  var selectionComparator: GenomeComparator
}