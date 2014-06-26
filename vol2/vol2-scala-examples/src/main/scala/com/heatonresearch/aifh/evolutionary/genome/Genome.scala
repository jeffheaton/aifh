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
package com.heatonresearch.aifh.evolutionary.genome

import com.heatonresearch.aifh.evolutionary.population.Population
import com.heatonresearch.aifh.evolutionary.species.Species
import com.heatonresearch.aifh.learning.MLMethod

/**
 * A genome is the basic blueprint for creating an phenome (organism) in Encog.
 * Some genomes also function as phenomes.
 */
trait Genome extends MLMethod {
  /**
   * Copy from the specified genome into this one.
   *
   * @param source The source genome.
   */
  def copy(source: Genome)

  /**
   * The adjusted score, this considers old-age penalties and youth
   * bonuses. If there are no such bonuses or penalties, this is the same as
   * the score.
   */
  var adjustedScore: Double

  /**
   * The birth generation (or iteration).
   */
  var birthGeneration: Int

  /**
   * The population that this genome belongs to.
   */
  var population: Population

  /**
   * The score for this genome.
   */
  var score: Double

  /**
   * @return Return the size of this genome. This size is a relative number
   *         that indicates the complexity of the genome.
   */
  def size: Int

  /**
   * The species for this genome.
   */
  var species: Species
}