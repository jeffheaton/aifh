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
package com.heatonresearch.aifh.examples.operations;

import com.heatonresearch.aifh.evolutionary.population.BasicPopulation;
import com.heatonresearch.aifh.evolutionary.population.Population;
import com.heatonresearch.aifh.evolutionary.train.EvolutionaryAlgorithm;
import com.heatonresearch.aifh.evolutionary.train.basic.BasicEA;
import com.heatonresearch.aifh.genetic.crossover.Splice;
import com.heatonresearch.aifh.genetic.crossover.SpliceNoRepeat;
import com.heatonresearch.aifh.genetic.genome.IntegerArrayGenome;
import com.heatonresearch.aifh.genetic.genome.IntegerArrayGenomeFactory;
import com.heatonresearch.aifh.learning.MLMethod;
import com.heatonresearch.aifh.learning.score.ScoreFunction;
import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

import java.util.Arrays;

/**
 * This example demonstrates crossover using a splice operator.  Both the repeat and non-repeat
 * variants are shown.
 * <p/>
 * Sample output:
 * <p/>
 * Crossover Splice
 * Parent 1: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
 * Parent 2: [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
 * Offspring 1: [1, 2, 3, 7, 6, 5, 4, 3, 2, 10]
 * Offspring 2: [10, 9, 8, 4, 5, 6, 7, 8, 9, 1]
 * Crossover Splice No Repeat
 * Parent 1: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
 * Parent 2: [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
 * Offspring 1: [1, 8, 9, 7, 6, 5, 4, 3, 2, 10]
 * Offspring 2: [10, 3, 2, 4, 5, 6, 7, 8, 9, 1]
 */
public class CrossoverExample {
    /**
     * Demonstrate the crossover splice operator.  Two offspring will be created by swapping three
     * segments of the parents (two cut points). Some genes may repeat.
     */
    public static void splice() {
        System.out.println("Crossover Splice");

        // Create a random number generator
        GenerateRandom rnd = new MersenneTwisterGenerateRandom();

        // Create a new population.
        Population pop = new BasicPopulation();
        pop.setGenomeFactory(new IntegerArrayGenomeFactory(10));

        // Create a trainer with a very simple score function.  We do not care
        // about the calculation of the score, as they will never be calculated.
        EvolutionaryAlgorithm train = new BasicEA(pop, new ScoreFunction() {
            @Override
            public double calculateScore(MLMethod method) {
                return 0;
            }

            @Override
            public boolean shouldMinimize() {
                return false;
            }
        });

        // Create a splice operator, length = 5.  Use it 1.0 (100%) of the time.
        Splice opp = new Splice(5);
        train.addOperation(1.0, opp);

        // Create two parents, the genes are set to 1,2,3,4,5,7,8,9,10
        // and 10,9,8,7,6,5,4,3,2,1.
        IntegerArrayGenome[] parents = new IntegerArrayGenome[2];
        parents[0] = (IntegerArrayGenome) pop.getGenomeFactory().factor();
        parents[1] = (IntegerArrayGenome) pop.getGenomeFactory().factor();
        for (int i = 1; i <= 10; i++) {
            parents[0].getData()[i - 1] = i;
            parents[1].getData()[i - 1] = 11 - i;
        }

        // Create an array to hold the offspring.
        IntegerArrayGenome[] offspring = new IntegerArrayGenome[2];

        // Perform the operation
        opp.performOperation(rnd, parents, 0, offspring, 0);

        // Display the results
        System.out.println("Parent 1: " + Arrays.toString(parents[0].getData()));
        System.out.println("Parent 2: " + Arrays.toString(parents[1].getData()));
        System.out.println("Offspring 1: " + Arrays.toString(offspring[0].getData()));
        System.out.println("Offspring 2: " + Arrays.toString(offspring[1].getData()));

    }

    /**
     * Demonstrate the crossover splice operator (no repeat).  Two offspring will be created by
     * swapping three segments of the parents (two cut points). No repeated genes allowed per offspring.
     */
    public static void spliceNoRepeat() {
        System.out.println("Crossover Splice No Repeat");

        // Create a random number generator
        GenerateRandom rnd = new MersenneTwisterGenerateRandom();

        // Create a new population.
        Population pop = new BasicPopulation();
        pop.setGenomeFactory(new IntegerArrayGenomeFactory(10));

        // Create a trainer with a very simple score function.  We do not care
        // about the calculation of the score, as they will never be calculated.
        EvolutionaryAlgorithm train = new BasicEA(pop, new ScoreFunction() {
            @Override
            public double calculateScore(MLMethod method) {
                return 0;
            }

            @Override
            public boolean shouldMinimize() {
                return false;
            }
        });

        // Create a splice (no repeat) operator, length = 5.  Use it 1.0 (100%) of the time.
        SpliceNoRepeat opp = new SpliceNoRepeat(5);
        train.addOperation(1.0, opp);

        // Create two parents, the genes are set to 1,2,3,4,5,7,8,9,10
        // and 10,9,8,7,6,5,4,3,2,1.
        IntegerArrayGenome[] parents = new IntegerArrayGenome[2];
        parents[0] = (IntegerArrayGenome) pop.getGenomeFactory().factor();
        parents[1] = (IntegerArrayGenome) pop.getGenomeFactory().factor();
        for (int i = 1; i <= 10; i++) {
            parents[0].getData()[i - 1] = i;
            parents[1].getData()[i - 1] = 11 - i;
        }

        // Create an array to hold the offspring.
        IntegerArrayGenome[] offspring = new IntegerArrayGenome[2];

        // Perform the operation
        opp.performOperation(rnd, parents, 0, offspring, 0);

        // Display the results
        System.out.println("Parent 1: " + Arrays.toString(parents[0].getData()));
        System.out.println("Parent 2: " + Arrays.toString(parents[1].getData()));
        System.out.println("Offspring 1: " + Arrays.toString(offspring[0].getData()));
        System.out.println("Offspring 2: " + Arrays.toString(offspring[1].getData()));

    }

    /**
     * Main entry point.
     *
     * @param args Not used.
     */
    public static void main(String[] args) {
        splice();
        spliceNoRepeat();
    }
}
