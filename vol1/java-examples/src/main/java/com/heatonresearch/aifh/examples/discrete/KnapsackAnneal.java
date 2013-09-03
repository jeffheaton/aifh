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
import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 8/26/13
 * Time: 11:28 AM
 * To change this template use File | Settings | File Templates.
 */
public class KnapsackAnneal extends DiscreteAnneal {
    /**
     * Number of items to choose from.
     */
    public static int NUM_ITEMS_TO_CHOOSE = 25;

    /**
     * The max weight of the knapsack.
     */
    public static int KNAPSACK_MAX_WEIGHT = 50;

    public static int ITEM_MAX_WEIGHT = 20;
    public static int ITEM_MAX_VALUE = 1000;

    private int[] profit = new int[NUM_ITEMS_TO_CHOOSE + 1];
    private int[] weight = new int[NUM_ITEMS_TO_CHOOSE + 1];

    private boolean[] currentTaken;
    private boolean[] backupTaken;
    private boolean[] bestTaken;
    private GenerateRandom rnd = new MersenneTwisterGenerateRandom();

    public KnapsackAnneal() {
        super(false, 1000, 40000, 0.001);

        this.currentTaken = new boolean[NUM_ITEMS_TO_CHOOSE];
        this.backupTaken = new boolean[NUM_ITEMS_TO_CHOOSE];
        this.bestTaken = new boolean[NUM_ITEMS_TO_CHOOSE];

        for (int i = 0; i < this.currentTaken.length; i++) {
            this.currentTaken[i] = this.rnd.nextBoolean();
        }
        balance();
    }

    public void run() {

        // Generate a random set of items.
        for (int n = 0; n < NUM_ITEMS_TO_CHOOSE; n++) {
            profit[n] = (int) (Math.random() * ITEM_MAX_VALUE);
            weight[n] = (int) (Math.random() * ITEM_MAX_WEIGHT);
        }

        // now begin main loop, and find a minimum
        while (!done()) {
            this.iteration();
            System.out.println("Iteration #" + getK() + ", Best Score=" + this.getBestScore() + "," + getStatus());
        }


        // print results
        System.out.println("item" + "\t" + "profit" + "\t" + "weight" + "\t" + "take");
        for (int n = 0; n < NUM_ITEMS_TO_CHOOSE; n++) {
            System.out.println((n + 1) + "\t" + profit[n] + "\t" + weight[n] + "\t" + bestTaken[n]);
        }
    }

    @Override
    public void backupState() {
        System.arraycopy(this.currentTaken, 0, this.backupTaken, 0, this.currentTaken.length);
    }

    @Override
    public void restoreState() {
        System.arraycopy(this.backupTaken, 0, this.currentTaken, 0, this.currentTaken.length);
    }

    @Override
    public void foundNewBest() {
        System.arraycopy(this.currentTaken, 0, this.bestTaken, 0, this.currentTaken.length);
    }

    @Override
    public void moveToNeighbor() {

        // check for strange case where we have everything!
        // This means that the max allowed knapsack weight is greater than the total of grabbing everything.
        // This is kind of pointless, but don't go into an endless loop!
        boolean holdingEverythingAlready = true;
        for (int i = 0; i < this.currentTaken.length; i++) {
            if (!this.currentTaken[i]) {
                holdingEverythingAlready = false;
                break;
            }
        }

        if (!holdingEverythingAlready) {
            // try to add something
            int pt = this.rnd.nextInt(this.currentTaken.length); // prime
            while (this.currentTaken[pt]) {
                pt = this.rnd.nextInt(this.currentTaken.length);
            }

            // add the item we found
            this.currentTaken[pt] = true;

            // We probably need to drop something now.
            balance();
        }

    }

    @Override
    public double evaluate() {
        if (calculateTotalWeight() > KNAPSACK_MAX_WEIGHT) {
            return 0;
        }

        int result = 0;
        for (int i = 0; i < this.currentTaken.length; i++) {
            if (this.currentTaken[i]) {
                result += this.profit[i];
            }
        }
        return result;
    }

    private int calculateTotalWeight() {
        int result = 0;
        for (int i = 0; i < this.currentTaken.length; i++) {
            if (this.currentTaken[i]) {
                result += this.weight[i];
            }
        }
        return result;
    }

    private void balance() {
        while (calculateTotalWeight() > KNAPSACK_MAX_WEIGHT) {
            int remove = rnd.nextInt(this.currentTaken.length);
            this.currentTaken[remove] = false;
        }
    }

    public static void main(String[] args) {
        KnapsackAnneal prg = new KnapsackAnneal();
        prg.run();
    }
}
