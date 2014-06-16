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
import com.heatonresearch.aifh.evolutionary.codec.GeneticCODEC;
import com.heatonresearch.aifh.evolutionary.genome.Genome;
import com.heatonresearch.aifh.evolutionary.population.Population;
import com.heatonresearch.aifh.evolutionary.score.AdjustScore;
import com.heatonresearch.aifh.evolutionary.species.Species;
import com.heatonresearch.aifh.learning.score.ScoreFunction;

import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

/**
 * This class is used to calculate the scores for an entire population. This is
 * typically done when a new population must be scored for the first time.
 */
public class ParallelScore {
    /**
     * The population to score.
     */
    private final Population population;

    /**
     * The CODEC used to create genomes.
     */
    private final GeneticCODEC codec;

    /**
     * The scoring function.
     */
    private final ScoreFunction scoreFunction;

    /**
     * The score adjuster.
     */
    private final List<AdjustScore> adjusters;

    /**
     * The number of requested threads.
     */
    private int threads;

    /**
     * The actual number of threads.
     */
    private int actualThreads;

    /**
     * Construct the parallel score calculation object.
     *
     * @param thePopulation    The population to score.
     * @param theCODEC         The CODEC to use.
     * @param theAdjusters     The score adjusters to use.
     * @param theScoreFunction The score function.
     * @param theThreadCount   The requested thread count.
     */
    public ParallelScore(Population thePopulation, GeneticCODEC theCODEC,
                         List<AdjustScore> theAdjusters, ScoreFunction theScoreFunction,
                         int theThreadCount) {
        this.codec = theCODEC;
        this.population = thePopulation;
        this.scoreFunction = theScoreFunction;
        this.adjusters = theAdjusters;
        this.actualThreads = theThreadCount;
    }

    /**
     * @return the population
     */
    public Population getPopulation() {
        return population;
    }

    /**
     * @return the scoreFunction
     */
    public ScoreFunction getScoreFunction() {
        return scoreFunction;
    }

    /**
     * @return the codec
     */
    public GeneticCODEC getCodec() {
        return codec;
    }

    /**
     * Calculate the scores.
     */
    public void process() {
        // determine thread usage
        if (threads == 0) {
            this.actualThreads = Runtime.getRuntime().availableProcessors();
        } else {
            this.actualThreads = threads;
        }

        // start up
        ExecutorService taskExecutor;

        if (this.threads == 1) {
            taskExecutor = Executors.newSingleThreadScheduledExecutor();
        } else {
            taskExecutor = Executors.newFixedThreadPool(this.actualThreads);
        }

        for (Species species : this.population.getSpecies()) {
            for (Genome genome : species.getMembers()) {
                taskExecutor.execute(new ParallelScoreTask(genome, this));
            }
        }

        taskExecutor.shutdown();
        try {
            taskExecutor.awaitTermination(Long.MAX_VALUE, TimeUnit.MINUTES);
        } catch (InterruptedException e) {
            throw new AIFHError(e);
        }
    }

    /**
     * @return The score adjusters.
     */
    public List<AdjustScore> getAdjusters() {
        return this.adjusters;
    }

    /**
     * @return The desired number of threads.
     */
    public int getThreadCount() {
        return this.threads;
    }

    /**
     * @param numThreads The desired thread count.
     */
    public void setThreadCount(int numThreads) {
        this.threads = numThreads;
    }
}
