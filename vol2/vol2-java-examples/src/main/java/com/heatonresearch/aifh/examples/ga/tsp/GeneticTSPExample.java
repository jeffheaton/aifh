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

import com.heatonresearch.aifh.evolutionary.population.BasicPopulation;
import com.heatonresearch.aifh.evolutionary.population.Population;
import com.heatonresearch.aifh.evolutionary.species.BasicSpecies;
import com.heatonresearch.aifh.evolutionary.train.basic.BasicEA;
import com.heatonresearch.aifh.genetic.crossover.SpliceNoRepeat;
import com.heatonresearch.aifh.genetic.genome.IntegerArrayGenome;
import com.heatonresearch.aifh.genetic.genome.IntegerArrayGenomeFactory;
import com.heatonresearch.aifh.genetic.mutate.MutateShuffle;
import com.heatonresearch.aifh.learning.score.ScoreFunction;

/**
 * Find the shortest path through several cities with a genetic algorithm (GA).
 * This example shows how to use it to find a potential solution to the Traveling Salesman Problem (TSP).
 */
public class GeneticTSPExample {
    /**
     * The number of cities to visit.
     */
    public static final int CITIES = 50;

    /**
     * The size of the population.
     */
    public static final int POPULATION_SIZE = 1000;

    /**
     * The square size of the map.
     */
    public static final int MAP_SIZE = 256;

    /**
     * The maximum number of iterations to allow to have the same score before giving up.
     */
    public static final int MAX_SAME_SOLUTION = 50;

    /**
     * The genetic algorithm.
     */
    private BasicEA genetic;

    /**
     * The cities to visit.
     */
    private City cities[];

    /**
     * Place the cities in random locations.
     */
    private void initCities() {
        cities = new City[CITIES];
        for (int i = 0; i < cities.length; i++) {
            int xPos = (int) (Math.random() * MAP_SIZE);
            int yPos = (int) (Math.random() * MAP_SIZE);

            cities[i] = new City(xPos, yPos);
        }
    }

    /**
     * Generate a random path through cities.
     */
    private IntegerArrayGenome randomGenome() {
        IntegerArrayGenome result = new IntegerArrayGenome(cities.length);
        final int organism[] = result.getData();
        final boolean taken[] = new boolean[cities.length];

        for (int i = 0; i < organism.length - 1; i++) {
            int icandidate;
            do {
                icandidate = (int) (Math.random() * organism.length);
            } while (taken[icandidate]);
            organism[i] = icandidate;
            taken[icandidate] = true;
            if (i == organism.length - 2) {
                icandidate = 0;
                while (taken[icandidate]) {
                    icandidate++;
                }
                organism[i + 1] = icandidate;
            }
        }
        return result;
    }

    /**
     * Create an initial random population of random paths through the cities.
     *
     * @return The random population.
     */
    private Population initPopulation() {
        Population result = new BasicPopulation(POPULATION_SIZE, null);

        BasicSpecies defaultSpecies = new BasicSpecies();
        defaultSpecies.setPopulation(result);
        for (int i = 0; i < POPULATION_SIZE; i++) {
            final IntegerArrayGenome genome = randomGenome();
            defaultSpecies.add(genome);
        }
        result.setGenomeFactory(new IntegerArrayGenomeFactory(cities.length));
        result.getSpecies().add(defaultSpecies);

        return result;
    }


    /**
     * Display the cities in the final path.
     */
    public void displaySolution(IntegerArrayGenome solution) {

        boolean first = true;
        int[] path = solution.getData();

        for (final int aPath : path) {
            if (!first)
                System.out.print(">");
            System.out.print("" + aPath);
            first = false;
        }

        System.out.println();
    }

    /**
     * Setup and solve the TSP.
     */
    public void solve() {
        StringBuilder builder = new StringBuilder();

        initCities();

        Population pop = initPopulation();

        ScoreFunction score = new TSPScore(cities);

        genetic = new BasicEA(pop, score);

        genetic.addOperation(0.9, new SpliceNoRepeat(CITIES / 3));
        genetic.addOperation(0.1, new MutateShuffle());

        int sameSolutionCount = 0;
        int iteration = 1;
        double lastSolution = Double.MAX_VALUE;

        while (sameSolutionCount < MAX_SAME_SOLUTION) {
            genetic.iteration();

            double thisSolution = genetic.getLastError();

            builder.setLength(0);
            builder.append("Iteration: ");
            builder.append(iteration++);
            builder.append(", Best Path Length = ");
            builder.append(thisSolution);

            System.out.println(builder.toString());

            if (Math.abs(lastSolution - thisSolution) < 1.0) {
                sameSolutionCount++;
            } else {
                sameSolutionCount = 0;
            }

            lastSolution = thisSolution;
        }

        System.out.println("Good solution found:");
        IntegerArrayGenome best = (IntegerArrayGenome) genetic.getBestGenome();
        displaySolution(best);
        genetic.finishTraining();

    }

    /**
     * Program entry point.
     *
     * @param args Not used.
     */
    public static void main(String args[]) {
        GeneticTSPExample solve = new GeneticTSPExample();
        solve.solve();
    }
}
