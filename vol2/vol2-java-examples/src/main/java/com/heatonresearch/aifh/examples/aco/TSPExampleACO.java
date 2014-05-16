package com.heatonresearch.aifh.examples.aco;

import com.heatonresearch.aifh.aco.CostGraph;
import com.heatonresearch.aifh.aco.DiscreteACO;
import com.heatonresearch.aifh.examples.ga.tsp.City;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 5/7/14
 * Time: 4:14 AM
 * To change this template use File | Settings | File Templates.
 */
public class TSPExampleACO implements CostGraph {
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
     * Display the cities in the final path.
     */
    public void displaySolution(int[] path) {

        boolean first = true;

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
        DiscreteACO aco = new DiscreteACO(this,50);

        int sameSolutionCount = 0;
        int iteration = 1;
        double lastSolution = Double.MAX_VALUE;

        while (sameSolutionCount < MAX_SAME_SOLUTION) {
            aco.iteration();

            double thisSolution = aco.getBestCost();

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
        int[] best = aco.getBestTour();
        displaySolution(best);
    }

    /**
     * Program entry point.
     * @param args Not used.
     */
    public static void main(String args[]) {
        TSPExampleACO solve = new TSPExampleACO();
        solve.solve();
    }

    @Override
    public double cost(final int sourceNode, final int targetNode) {
        City city1 = cities[sourceNode];
        City city2 = cities[targetNode];
        return city1.proximity(city2);
    }

    @Override
    public int graphSize() {
        return CITIES;
    }
}
