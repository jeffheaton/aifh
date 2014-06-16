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
package com.heatonresearch.aifh.examples.gp;

import com.heatonresearch.aifh.evolutionary.population.BasicPopulation;
import com.heatonresearch.aifh.evolutionary.population.Population;
import com.heatonresearch.aifh.evolutionary.species.BasicSpecies;
import com.heatonresearch.aifh.evolutionary.train.EvolutionaryAlgorithm;
import com.heatonresearch.aifh.evolutionary.train.basic.BasicEA;
import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.genetic.trees.CrossoverTree;
import com.heatonresearch.aifh.genetic.trees.MutateTree;
import com.heatonresearch.aifh.genetic.trees.TreeGenome;
import com.heatonresearch.aifh.genetic.trees.TreeGenomeFactory;
import com.heatonresearch.aifh.learning.score.ScoreFunction;
import com.heatonresearch.aifh.learning.score.ScoreRegressionData;
import com.heatonresearch.aifh.normalize.DataSet;
import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;

/**
 * An example that fits an equation to a data file.  This example uses genetic programming.
 */
public class FindEquation {

    /**
     * The size of the population.
     */
    public static final int POPULATION_SIZE = 1000;

    /**
     * The maximum number of iterations to allow to have the same score before giving up.
     */
    public static final int MAX_SAME_SOLUTION = 500;

    /**
     * Generate a random path through cities.
     */
    private TreeGenome randomGenome(GenerateRandom rnd, EvaluateExpression eval) {
        TreeGenome result = new TreeGenome(eval);
        result.setRoot(eval.grow(rnd, 5));
        return result;
    }

    /**
     * Create an initial random population.
     *
     * @param rnd  A random number generator.
     * @param eval The expression evaluator.
     * @return The new population.
     */
    private Population initPopulation(GenerateRandom rnd, EvaluateExpression eval) {
        Population result = new BasicPopulation(POPULATION_SIZE, null);

        BasicSpecies defaultSpecies = new BasicSpecies();
        defaultSpecies.setPopulation(result);
        for (int i = 0; i < POPULATION_SIZE; i++) {
            final TreeGenome genome = randomGenome(rnd, eval);
            defaultSpecies.add(genome);
        }
        result.setGenomeFactory(new TreeGenomeFactory(eval));
        result.getSpecies().add(defaultSpecies);

        return result;
    }

    /**
     * Process the specified file.
     *
     * @param filename The filename to process.
     */
    public void process(final String filename) {
        InputStream istream = null;

        // If no file is provided, try to use the simple polynomial data from the resources.
        if (filename == null) {
            istream = this.getClass().getResourceAsStream("/simple-poly.csv");
            if (istream == null) {
                System.out.println("Cannot access data set, make sure the resources are available.");
                System.exit(1);
            }
        } else {
            // If a file is provided, try to read from that file.
            try {
                istream = new FileInputStream(filename);
            } catch (IOException ex) {
                ex.printStackTrace();
                System.exit(1);
            }
        }

        // Load the file and obtain training data.
        final DataSet ds = DataSet.load(istream);
        // Extract supervised training.
        List<BasicData> training = ds.extractSupervised(0, 1, 1, 1);


        GenerateRandom rnd = new MersenneTwisterGenerateRandom();
        EvaluateExpression eval = new EvaluateExpression(rnd);
        Population pop = initPopulation(rnd, eval);
        ScoreFunction score = new ScoreSmallExpression(training,30);

        EvolutionaryAlgorithm genetic = new BasicEA(pop, score);
        genetic.addOperation(0.3, new MutateTree(3));
        genetic.addOperation(0.7, new CrossoverTree());
        genetic.setShouldIgnoreExceptions(false);


        int sameSolutionCount = 0;
        int iteration = 1;
        double lastSolution = Double.MAX_VALUE;
        StringBuilder builder = new StringBuilder();

        while (sameSolutionCount < MAX_SAME_SOLUTION && iteration<1000) {
            genetic.iteration();

            double thisSolution = genetic.getLastError();

            builder.setLength(0);
            builder.append("Iteration: ");
            builder.append(iteration++);
            builder.append(", Current error = ");
            builder.append(thisSolution);
            builder.append(", Best Solution Length = ");
            builder.append(genetic.getBestGenome().size());

            System.out.println(builder.toString());

            if (Math.abs(lastSolution - thisSolution) < 1.0) {
                sameSolutionCount++;
            } else {
                sameSolutionCount = 0;
            }

            lastSolution = thisSolution;
        }

        System.out.println("Good solution found:");
        TreeGenome best = (TreeGenome) genetic.getBestGenome();
        System.out.println(eval.displayExpressionNormal(best.getRoot()));
        genetic.finishTraining();
    }

    /**
     * Main entry point.
     *
     * @param args The data file to fit.
     */
    public static void main(String[] args) {
        FindEquation prg = new FindEquation();
        if (args.length == 0) {
            prg.process(null);
        } else if (args.length == 1) {
            prg.process(args[0]);
        } else {
            System.out.println("Specify a filename to fit, or no filename to use a built in simple polynomial.");
            System.exit(1);
        }


    }
}
