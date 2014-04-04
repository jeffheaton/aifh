package com.heatonresearch.aifh.randomize;

import com.heatonresearch.aifh.AIFHError;

import java.io.Serializable;

/**
 * Generate random choices unevenly.  This class is used to select random
 * choices from a list, with a probability weight places on each item
 * in the list.
 *
 * This is often called a Roulette Wheel in Machine Learning texts.  How it differs from
 * a Roulette Wheel that you might find in Las Vegas or Monte Carlo is that the
 * areas that can be selected are not of uniform size.  However, you can be sure
 * that one will be picked.
 *
 * http://en.wikipedia.org/wiki/Fitness_proportionate_selection
 */
public class RandomChoice implements Serializable {
    /**
     * The probabilities of each item in the list.
     */
    final private double[] probabilities;

    /**
     * Construct a list of probabilities.
     * @param theProbabilities The probability of each item in the list.
     */
    public RandomChoice(double[] theProbabilities) {

        this.probabilities = theProbabilities.clone();

        double total = 0;
        for (int i = 0; i < probabilities.length; i++) {
            total += probabilities[i];
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
