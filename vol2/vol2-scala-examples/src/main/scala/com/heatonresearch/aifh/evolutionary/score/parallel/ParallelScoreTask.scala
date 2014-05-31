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
package com.heatonresearch.aifh.evolutionary.score.parallel

import com.heatonresearch.aifh.AIFHError
import com.heatonresearch.aifh.evolutionary.genome.Genome
import com.heatonresearch.aifh.evolutionary.score.AdjustScore
import com.heatonresearch.aifh.evolutionary.train.basic.BasicEA
import com.heatonresearch.aifh.learning.MLMethod
import com.heatonresearch.aifh.learning.score.ScoreFunction

/**
 * An individual threadable task for the parallel score calculation.
 * @param genome   The genome to calculate the score for.
 * @param owner The owner.
 */
class ParallelScoreTask(val genome: Genome,val owner: ParallelScore) extends Runnable {

  /**
   * The score function.
   */
  private val scoreFunction: ScoreFunction = owner.scoreFunction
  /**
   * The score adjusters.
   */
  private val adjusters: List[AdjustScore] = owner.adjusters

  /**
   * Perform the task.
   */
  def run(): Unit = {
    val phenotype = owner.codec.decode(genome)
    if (phenotype != null) {
      val score =
        try {
          scoreFunction.calculateScore(phenotype)
        } catch {
          case e: AIFHError =>
            Double.NaN
        }
      genome.score = score
      genome.adjustedScore = score
      BasicEA.calculateScoreAdjustment(genome, adjusters)
    }
  }
}