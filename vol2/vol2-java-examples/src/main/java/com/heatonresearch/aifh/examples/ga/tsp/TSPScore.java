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
     * @param cities The cities.
     */
    public TSPScore(City[] cities)
    {
        this.cities = cities;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double calculateScore(MLMethod phenotype) {
        double result = 0.0;
        IntegerArrayGenome genome = (IntegerArrayGenome) phenotype;
        int[] path = ((IntegerArrayGenome)genome).getData();

        for (int i = 0; i < cities.length - 1; i++) {
            City city1 = cities[path[i]];
            City city2 = cities[path[i+1]];

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

