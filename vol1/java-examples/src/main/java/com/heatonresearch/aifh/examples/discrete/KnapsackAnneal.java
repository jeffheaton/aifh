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
 * This example program shows how to use discrete simulated annealing to find solutions to the Knapsack problem.
 * <p/>
 * http://en.wikipedia.org/wiki/Knapsack_problem
 */
public class KnapsackAnneal extends DiscreteAnneal {
    /**
     * Number of items to choose from.
     */
    public static final int NUM_ITEMS_TO_CHOOSE = 25;

    /**
     * The max weight of the knapsack.
     */
    public static final int KNAPSACK_MAX_WEIGHT = 50;

    /**
     * The max weight for an item.
     */
    public static final int ITEM_MAX_WEIGHT = 20;

    /**
     * The max value for an item.
     */
    public static final int ITEM_MAX_VALUE = 1000;

    /**
     * The profit for each item.
     */
    private final int[] profit = new int[NUM_ITEMS_TO_CHOOSE + 1];

    /**
     * The weight for each item.
     */
    private final int[] weight = new int[NUM_ITEMS_TO_CHOOSE + 1];

    /**
     * The current items taken.
     */
    private final boolean[] currentTaken;

    /**
     * A backup of the items taken, in case we need to revert.
     */
    private final boolean[] backupTaken;

    /**
     * The best set of items so far.
     */
    private final boolean[] bestTaken;

    /**
     * A random number generator.
     */
    private final GenerateRandom rnd = new MersenneTwisterGenerateRandom();

    /**
     * Construct the object and init.
     */
    public KnapsackAnneal() {
        super(1000, 40000, 0.001);

        this.currentTaken = new boolean[NUM_ITEMS_TO_CHOOSE];
        this.backupTaken = new boolean[NUM_ITEMS_TO_CHOOSE];
        this.bestTaken = new boolean[NUM_ITEMS_TO_CHOOSE];

        for (int i = 0; i < this.currentTaken.length; i++) {
            this.currentTaken[i] = this.rnd.nextBoolean();
        }
        balance();
    }

    /**
     * Run the example.
     */
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

    /**
     * {@inheritDoc}
     */
    @Override
    public void backupState() {
        System.arraycopy(this.currentTaken, 0, this.backupTaken, 0, this.currentTaken.length);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void restoreState() {
        System.arraycopy(this.backupTaken, 0, this.currentTaken, 0, this.currentTaken.length);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void foundNewBest() {
        System.arraycopy(this.currentTaken, 0, this.bestTaken, 0, this.currentTaken.length);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void moveToNeighbor() {

        // check for strange case where we have everything!
        // This means that the max allowed knapsack weight is greater than the total of grabbing everything.
        // This is kind of pointless, but don't go into an endless loop!
        boolean holdingEverythingAlready = true;
        for (final boolean aCurrentTaken : this.currentTaken) {
            if (!aCurrentTaken) {
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

    /**
     * {@inheritDoc}
     */
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

    /**
     * @return The total weight.
     */
    private int calculateTotalWeight() {
        int result = 0;
        for (int i = 0; i < this.currentTaken.length; i++) {
            if (this.currentTaken[i]) {
                result += this.weight[i];
            }
        }
        return result;
    }

    /**
     * Balance and keep below max weight.
     */
    private void balance() {
        while (calculateTotalWeight() > KNAPSACK_MAX_WEIGHT) {
            final int remove = rnd.nextInt(this.currentTaken.length);
            this.currentTaken[remove] = false;
        }
    }

    /**
     * The main method.
     *
     * @param args Not used.
     */
    public static void main(final String[] args) {
        final KnapsackAnneal prg = new KnapsackAnneal();
        prg.run();
    }
}
