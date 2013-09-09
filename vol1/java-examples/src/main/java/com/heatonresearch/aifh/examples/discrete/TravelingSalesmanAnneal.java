/*
 * Artificial Intelligence for Humans
 * Volume 1: Fundamental Algorithms
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh

 * Copyright 2013 by Jeff Heaton
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

package com.heatonresearch.aifh.examples.discrete;

import com.heatonresearch.aifh.discrete.DiscreteAnneal;
import com.heatonresearch.aifh.distance.CalculateDistance;
import com.heatonresearch.aifh.distance.EuclideanDistance;
import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

import java.util.Arrays;

/**
 * Use simulated annealing with the Traveling Salesman Problem (TSP).  The cities are placed in a circle, so the
 * ideal path is known.  Because the cities are in a circle they should be visited in order for the absolute
 * optimal path.
 * <p/>
 * http://en.wikipedia.org/wiki/Traveling_salesman_problem
 */
public class TravelingSalesmanAnneal extends DiscreteAnneal {

    /**
     * The size of the map.
     */
    public static final double MAP_SIZE = 10;

    /**
     * The city count.
     */
    public static final int CITY_COUNT = 50;

    /**
     * The distance calculator.
     */
    private final CalculateDistance distance = new EuclideanDistance();

    /**
     * The city coordinates.
     */
    private double[][] cities;

    /**
     * A random number generator.
     */
    private final GenerateRandom rnd = new MersenneTwisterGenerateRandom();

    /**
     * The current path being evaluated.
     */
    private int[] currentPath;

    /**
     * The backup path, in case the current is not kept.
     */
    private int[] backupPath;

    /**
     * The best path yet.
     */
    private int[] bestPath;

    /**
     * Construct the object.
     */
    public TravelingSalesmanAnneal() {
        super(1000, 400, 0.001);
    }

    /**
     * Run the example.
     */
    public void run() {
        this.cities = new double[CITY_COUNT][2];
        this.currentPath = new int[CITY_COUNT];
        this.backupPath = new int[CITY_COUNT];
        this.bestPath = new int[CITY_COUNT];

        // place the cities in a circle
        final double ratio = (2 * Math.PI) / this.cities.length;

        for (int cityNumber = 0; cityNumber < cities.length; cityNumber++) {
            this.cities[cityNumber][0] = (int) (Math.cos(ratio * cityNumber) * (MAP_SIZE / 2) + (MAP_SIZE / 2));
            this.cities[cityNumber][1] = (int) (Math.sin(ratio * cityNumber) * (MAP_SIZE / 2) + (MAP_SIZE / 2));
        }

        // pick a random city order
        this.currentPath = new int[CITY_COUNT];
        for (int i = 0; i < this.currentPath.length; i++) {
            int city;
            boolean foundCity;

            do {
                city = this.rnd.nextInt(CITY_COUNT);
                foundCity = false;
                for (int j = 0; j < i; j++) {
                    if (city == this.currentPath[j]) {
                        foundCity = true;
                    }
                }
            } while (foundCity);

            this.currentPath[i] = city;
        }

        // now begin main loop, and find a minimum
        while (!done()) {
            this.iteration();
            System.out.println("Iteration #" + getK() + ", Best Score=" + this.getBestScore() + "," + getStatus());
        }

        System.out.println(Arrays.toString(this.bestPath));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void backupState() {
        System.arraycopy(this.currentPath, 0, this.backupPath, 0, this.currentPath.length);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void restoreState() {
        System.arraycopy(this.backupPath, 0, this.currentPath, 0, this.currentPath.length);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void foundNewBest() {
        System.arraycopy(this.currentPath, 0, this.bestPath, 0, this.currentPath.length);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void moveToNeighbor() {

        // pick the first point to swap
        final int pt1 = this.rnd.nextInt(this.currentPath.length);

        // pick the second point to swap, can't be the same as the first
        int pt2;

        do {
            pt2 = this.rnd.nextInt(this.currentPath.length);
        } while (pt1 == pt2);

        // swap them
        final int temp = this.currentPath[pt1];
        this.currentPath[pt1] = this.currentPath[pt2];
        this.currentPath[pt2] = temp;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double evaluate() {
        double result = 0;
        for (int i = 0; i < (cities.length - 1); i++) {
            // find current and next city
            final double[] city1 = this.cities[this.currentPath[i]];
            final double[] city2 = this.cities[this.currentPath[i + 1]];
            result += this.distance.calculate(city1, city2);
        }

        return result;
    }

    /**
     * The main function.
     *
     * @param args Not used.
     */
    public static void main(final String[] args) {
        final TravelingSalesmanAnneal prg = new TravelingSalesmanAnneal();
        prg.run();
    }
}
