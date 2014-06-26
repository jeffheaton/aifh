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
import java.io.Serializable

/**
 * A basic abstract genome. Provides base functionality.
 */
@SerialVersionUID(1L)
abstract class BasicGenome extends Genome with Serializable {

  override def toString: String = s"[${getClass.getSimpleName}: score=$score"

  /**
   * The adjusted score. If unknown, it is set to NaN.
   */
  var adjustedScore: Double = Double.NaN
  /**
   * The score of this genome.
   */
  var score: Double = Double.NaN
  /**
   * The population this genome belongs to.
   */
  var population: Population = null
  /**
   * The birth generation for this genome.
   */
  var birthGeneration: Int = 0
  /**
   * The species of this genome.
   */
  var species: Species = null
}