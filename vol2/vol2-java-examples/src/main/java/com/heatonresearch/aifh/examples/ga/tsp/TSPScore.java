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
package com.heatonresearch.aifh.examples.ga.tsp;

import com.heatonresearch.aifh.genetic.genome.IntegerArrayGenome;
import com.heatonresearch.aifh.learning.MLMethod;
import com.heatonresearch.aifh.learning.score.ScoreFunction;

/**
 * Calculate a score for the TSP.
 */
public class TSPScore implements ScoreFunction {

    /**
     * The path of cities to visit.
     */
    private City[] cities;

    /**
     * The constructor.
     *
     * @param cities The cities.
     */
    public TSPScore(City[] cities) {
        this.cities = cities;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double calculateScore(MLMethod phenotype) {
        double result = 0.0;
        IntegerArrayGenome genome = (IntegerArrayGenome) phenotype;
        int[] path = genome.getData();

        for (int i = 0; i < cities.length - 1; i++) {
            City city1 = cities[path[i]];
            City city2 = cities[path[i + 1]];

            final double dist = city1.proximity(city2);
            result += dist;
        }

        return result;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean shouldMinimize() {
        return true;
    }
}

