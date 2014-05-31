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
package com.heatonresearch.aifh.evolutionary.sort

import com.heatonresearch.aifh.evolutionary.genome.Genome
import java.io.Serializable

/**
 * Use this comparator to minimize the adjusted score.
 */
@SerialVersionUID(1L)
class MinimizeAdjustedScoreComp extends AbstractGenomeComparator with Serializable {
  override def compare(p1: Genome, p2: Genome): Int = p1.adjustedScore.compare(p2.adjustedScore)

  override def isBetterThan(prg: Genome, betterThan: Genome): Boolean =
    isBetterThan(prg.adjustedScore, betterThan.adjustedScore)

  def shouldMinimize: Boolean = true
}