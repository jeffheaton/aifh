package com.heatonresearch.aifh.examples.ga;

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
 * Find the shortest path through several cities with a genetic algorithmn (GA).
 * This example shows how to use it
 * to find a solution to the Traveling Salesman Problem (TSP).  This
 * example does not use any sort of neural network.
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

    private Population initPopulation()
    {
        Population result = new BasicPopulation(POPULATION_SIZE, null);

        BasicSpecies defaultSpecies = new BasicSpecies();
        defaultSpecies.setPopulation(result);
        for (int i = 0; i < POPULATION_SIZE; i++) {
            final IntegerArrayGenome genome = randomGenome();
            defaultSpecies.getMembers().add(genome);
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

        for(int i=0;i<path.length;i++) {
            if( !first )
                System.out.print(">");
            System.out.print( ""+ path[i]);
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

        ScoreFunction score =  new TSPScore(cities);

        genetic = new BasicEA(pop,score);

        genetic.addOperation(0.9,new SpliceNoRepeat(CITIES/3));
        genetic.addOperation(0.1,new MutateShuffle());

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
        IntegerArrayGenome best = (IntegerArrayGenome)genetic.getBestGenome();
        displaySolution(best);
        genetic.finishTraining();

    }

    /**
     * Program entry point.
     * @param args Not used.
     */
    public static void main(String args[]) {
        GeneticTSPExample solve = new GeneticTSPExample();
        solve.solve();
    }
}