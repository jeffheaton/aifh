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
package com.heatonresearch.aifh.evolutionary.score.parallel;

import com.heatonresearch.aifh.AIFHError;
import com.heatonresearch.aifh.evolutionary.genome.Genome;
import com.heatonresearch.aifh.evolutionary.score.AdjustScore;
import com.heatonresearch.aifh.evolutionary.train.basic.BasicEA;
import com.heatonresearch.aifh.learning.MLMethod;
import com.heatonresearch.aifh.learning.score.ScoreFunction;

import java.util.List;

/**
 * An individual threadable task for the parallel score calculation.
 */
public class ParallelScoreTask implements Runnable {

    /**
     * The genome to calculate the score for.
     */
    private final Genome genome;

    /**
     * The score function.
     */
    private final ScoreFunction scoreFunction;

    /**
     * The score adjusters.
     */
    private final List<AdjustScore> adjusters;

    /**
     * The owners.
     */
    private final ParallelScore owner;

    /**
     * Construct the parallel task.
     *
     * @param genome   The genome.
     * @param theOwner The owner.
     */
    public ParallelScoreTask(Genome genome, ParallelScore theOwner) {
        super();
        this.owner = theOwner;
        this.genome = genome;
        this.scoreFunction = theOwner.getScoreFunction();
        this.adjusters = theOwner.getAdjusters();
    }

    /**
     * Perform the task.
     */
    @Override
    public void run() {
        MLMethod phenotype = this.owner.getCodec().decode(this.genome);
        if (phenotype != null) {
            double score;
            try {
                score = this.scoreFunction.calculateScore(phenotype);
            } catch (AIFHError e) {
                score = Double.NaN;
            }
            genome.setScore(score);
            genome.setAdjustedScore(score);
            BasicEA.calculateScoreAdjustment(genome, adjusters);
        }
    }

}
