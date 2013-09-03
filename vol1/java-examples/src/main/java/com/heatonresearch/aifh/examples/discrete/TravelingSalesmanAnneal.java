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
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 8/25/13
 * Time: 3:51 PM
 * To change this template use File | Settings | File Templates.
 */
public class TravelingSalesmanAnneal extends DiscreteAnneal {

    public static final double MAP_SIZE = 10;
    public static final int CITY_COUNT = 50;

    private final CalculateDistance distance = new EuclideanDistance();
    private double[][] cities;
    private final GenerateRandom rnd = new MersenneTwisterGenerateRandom();
    private int[] currentPath;
    private int[] backupPath;
    private int[] bestPath;


    public TravelingSalesmanAnneal() {
        super(true, 1000, 400, 0.001);
    }

    public void run() {
        this.cities = new double[CITY_COUNT][2];
        this.currentPath = new int[CITY_COUNT];
        this.backupPath = new int[CITY_COUNT];
        this.bestPath = new int[CITY_COUNT];

        // place the cities in a circle
        double ratio = (2 * Math.PI) / this.cities.length;

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

    @Override
    public void backupState() {
        System.arraycopy(this.currentPath, 0, this.backupPath, 0, this.currentPath.length);
    }

    @Override
    public void restoreState() {
        System.arraycopy(this.backupPath, 0, this.currentPath, 0, this.currentPath.length);
    }

    @Override
    public void foundNewBest() {
        System.arraycopy(this.currentPath, 0, this.bestPath, 0, this.currentPath.length);
    }

    @Override
    public void moveToNeighbor() {

        // pick the first point to swap
        int pt1 = this.rnd.nextInt(this.currentPath.length);

        // pick the second point to swap, can't be the same as the first
        int pt2;

        do {
            pt2 = this.rnd.nextInt(this.currentPath.length);
        } while (pt1 == pt2);

        // swap them
        int temp = this.currentPath[pt1];
        this.currentPath[pt1] = this.currentPath[pt2];
        this.currentPath[pt2] = temp;
    }

    @Override
    public double evaluate() {
        double result = 0;
        for (int i = 0; i < (cities.length - 1); i++) {
            // find current and next city
            double[] city1 = this.cities[this.currentPath[i]];
            double[] city2 = this.cities[this.currentPath[i + 1]];
            result += this.distance.calculate(city1, city2);
        }

        return result;
    }

    public static void main(String[] args) {
        TravelingSalesmanAnneal prg = new TravelingSalesmanAnneal();
        prg.run();
    }
}
