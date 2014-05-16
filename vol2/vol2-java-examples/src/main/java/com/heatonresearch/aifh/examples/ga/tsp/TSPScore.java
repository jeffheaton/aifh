package com.heatonresearch.aifh.examples.ga.tsp;

import com.heatonresearch.aifh.genetic.genome.IntegerArrayGenome;
import com.heatonresearch.aifh.learning.MLMethod;
import com.heatonresearch.aifh.learning.score.ScoreFunction;

public class TSPScore implements ScoreFunction {

    private City[] cities;

    public TSPScore(City[] cities)
    {
        this.cities = cities;
    }

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

    public boolean shouldMinimize() {
        return true;
    }
}

