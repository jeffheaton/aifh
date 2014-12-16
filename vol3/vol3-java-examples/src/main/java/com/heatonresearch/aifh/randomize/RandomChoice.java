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
package com.heatonresearch.aifh.randomize;

import com.heatonresearch.aifh.AIFHError;

import java.io.Serializable;

/**
 * Generate random choices unevenly.  This class is used to select random
 * choices from a list, with a probability weight places on each item
 * in the list.
 * <p/>
 * This is often called a Roulette Wheel in Machine Learning texts.  How it differs from
 * a Roulette Wheel that you might find in Las Vegas or Monte Carlo is that the
 * areas that can be selected are not of uniform size.  However, you can be sure
 * that one will be picked.
 * <p/>
 * http://en.wikipedia.org/wiki/Fitness_proportionate_selection
 */
public class RandomChoice implements Serializable {
    /**
     * The probabilities of each item in the list.
     */
    final private double[] probabilities;

    /**
     * Construct a list of probabilities.
     *
     * @param theProbabilities The probability of each item in the list.
     */
    public RandomChoice(double[] theProbabilities) {

        this.probabilities = theProbabilities.clone();

        double total = 0;
        for (final double probability : probabilities) {
            total += probability;
        }

        if (total == 0.0) {
            double prob = 1.0 / probabilities.length;
            for (int i = 0; i < probabilities.length; i++) {
                probabilities[i] = prob;
            }
        } else {
            double total2 = 0;
            double factor = 1.0 / total;
            for (int i = 0; i < probabilities.length; i++) {
                probabilities[i] = probabilities[i] * factor;
                total2 += probabilities[i];
            }

            if (Math.abs(1.0 - total2) > 0.02) {
                double prob = 1.0 / probabilities.length;
                for (int i = 0; i < probabilities.length; i++) {
                    probabilities[i] = prob;
                }
            }
        }
    }


    /**
     * Generate a random choice, based on the probabilities provided to the constructor.
     *
     * @return The random choice.
     */
    public int generate(GenerateRandom theGenerator) {
        double r = theGenerator.nextDouble();
        double sum = 0.0;

        for (int i = 0; i < probabilities.length; i++) {
            sum += probabilities[i];
            if (r < sum) {
                return i;
            }
        }

        for (int i = 0; i < probabilities.length; i++) {
            if (probabilities[i] != 0.0) {
                return i;
            }
        }

        throw new AIFHError("Invalid probabilities.");
    }

    /**
     * Generate a random choice, but skip one of the choices.
     *
     * @param skip The choice to skip.
     * @return The random choice.
     */
    public int generate(GenerateRandom theGenerator, int skip) {
        double totalProb = 1.0 - probabilities[skip];

        double throwValue = theGenerator.nextDouble() * totalProb;
        double accumulator = 0.0;

        for (int i = 0; i < skip; i++) {
            accumulator += probabilities[i];
            if (accumulator > throwValue) {
                return i;
            }
        }

        for (int i = skip + 1; i < probabilities.length; i++) {
            accumulator += probabilities[i];
            if (accumulator > throwValue) {
                return i;
            }
        }

        for (int i = 0; i < skip; i++) {
            if (probabilities[i] != 0.0) {
                return i;
            }
        }
        for (int i = skip + 1; i < probabilities.length; i++) {
            if (probabilities[i] != 0.0) {
                return i;
            }
        }

        return -1;
    }
}
