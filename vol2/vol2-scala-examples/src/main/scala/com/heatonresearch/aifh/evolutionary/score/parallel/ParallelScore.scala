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
import com.heatonresearch.aifh.evolutionary.codec.GeneticCODEC
import com.heatonresearch.aifh.evolutionary.population.Population
import com.heatonresearch.aifh.evolutionary.score.AdjustScore
import com.heatonresearch.aifh.learning.score.ScoreFunction
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit

/**
 * This class is used to calculate the scores for an entire population. This is
 * typically done when a new population must be scored for the first time.
 * @param population    The population to score.
 * @param codec         The CODEC to use.
 * @param adjusters     The score adjusters to use.
 * @param scoreFunction The score function.
 * @param requestedThreads   The requested thread count.
 */
class ParallelScore(val population: Population,
                    val codec: GeneticCODEC,
                    val adjusters: List[AdjustScore],
                    val scoreFunction: ScoreFunction,
                    var requestedThreads:  Int) {

  /**
   * The actual number of requestedThreads.
   */
  private var actualThreads: Int = 0

  /**
   * Calculate the scores.
   */
  def process() {
    if (requestedThreads == 0)
      actualThreads = Runtime.getRuntime.availableProcessors
    else
      actualThreads = requestedThreads

    val taskExecutor : ExecutorService =
      if (requestedThreads == 1)
        Executors.newSingleThreadScheduledExecutor
      else
        Executors.newFixedThreadPool(actualThreads)

    for (species <- population.speciesList ;
         genome <- species.members) {
        taskExecutor.execute(new ParallelScoreTask(genome, this))
    }

    taskExecutor.shutdown()
    try {
      taskExecutor.awaitTermination(Long.MaxValue, TimeUnit.MINUTES)
    } catch {
      case e: InterruptedException =>
        throw new AIFHError(e)
    }
  }
}