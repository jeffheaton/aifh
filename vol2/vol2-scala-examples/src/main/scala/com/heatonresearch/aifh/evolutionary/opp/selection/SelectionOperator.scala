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

import com.heatonresearch.aifh.evolutionary.species.Species
import com.heatonresearch.aifh.evolutionary.train.EvolutionaryAlgorithm
import com.heatonresearch.aifh.randomize.GenerateRandom

/**
 * Provides the interface to a selection operator. This allows genomes to be
 * selected for offspring production or elimination.
 */
trait SelectionOperator {
  /**
   * Selects an fit genome.
   *
   * @param rnd     A random number generator.
   * @param species The species to select the genome from.
   * @return The selected genome.
   */
  def performSelection(rnd: GenerateRandom, species: Species): Int

  /**
   * Selects an unfit genome.
   *
   * @param rnd     A random number generator.
   * @param species The species to select the genome from.
   * @return The selected genome.
   */
  def performAntiSelection(rnd: GenerateRandom, species: Species): Int

  var trainer: EvolutionaryAlgorithm
}