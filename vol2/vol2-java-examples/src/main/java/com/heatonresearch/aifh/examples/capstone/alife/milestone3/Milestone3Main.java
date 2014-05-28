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
package com.heatonresearch.aifh.examples.capstone.alife.milestone3;

import com.heatonresearch.aifh.evolutionary.population.BasicPopulation;
import com.heatonresearch.aifh.evolutionary.population.Population;
import com.heatonresearch.aifh.evolutionary.species.BasicSpecies;
import com.heatonresearch.aifh.evolutionary.train.basic.BasicEA;
import com.heatonresearch.aifh.examples.capstone.alife.milestone1.DisplayPlant;
import com.heatonresearch.aifh.examples.capstone.alife.milestone1.PlantUniverse;
import com.heatonresearch.aifh.examples.capstone.alife.milestone2.PlantGrowth;
import com.heatonresearch.aifh.examples.capstone.alife.milestone2.PlantPhysics;
import com.heatonresearch.aifh.genetic.crossover.Splice;
import com.heatonresearch.aifh.genetic.genome.DoubleArrayGenome;
import com.heatonresearch.aifh.genetic.genome.DoubleArrayGenomeFactory;
import com.heatonresearch.aifh.genetic.mutate.MutatePerturb;
import com.heatonresearch.aifh.genetic.species.ArraySpeciation;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

import javax.swing.*;
import java.util.Arrays;

/**
 * The third milestone in the project is to evolve genomes that produce green leafy plants.  The genomes
 * are scored based on how leafy their plants is.
 */
public class Milestone3Main extends JFrame implements Runnable {
    /**
     * The plant display.
     */
    private DisplayPlant display;

    /**
     * The universe.
     */
    private PlantUniverse universe;

    /**
     * The population.
     */
    private Population pop;

    /**
     * The score function.
     */
    private PlantScore score;

    /**
     * The genetic training.
     */
    private BasicEA genetic;

    /**
     * Random number generator.
     */
    private MersenneTwisterGenerateRandom rnd = new MersenneTwisterGenerateRandom();

    /**
     * @return A random genome.
     */
    private DoubleArrayGenome randomGenome() {
        DoubleArrayGenome genome = new DoubleArrayGenome(PlantUniverse.GENOME_SIZE);

        for (int i = 0; i < genome.size(); i++) {
            genome.getData()[i] = rnd.nextDouble(0, 1);
        }
        return genome;
    }

    /**
     * Create the initial random population.
     *
     * @return The population.
     */
    private Population initPopulation() {
        Population result = new BasicPopulation(PlantUniverse.POPULATION_SIZE, null);

        BasicSpecies defaultSpecies = new BasicSpecies();
        defaultSpecies.setPopulation(result);
        for (int i = 0; i < PlantUniverse.POPULATION_SIZE; i++) {
            final DoubleArrayGenome genome = randomGenome();
            defaultSpecies.add(genome);
        }
        result.setGenomeFactory(new DoubleArrayGenomeFactory(PlantUniverse.GENOME_SIZE));
        result.getSpecies().add(defaultSpecies);

        return result;
    }

    /**
     * The constructor.
     */
    public Milestone3Main() {
        this.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);


        this.pop = initPopulation();
        this.score = new PlantScore();
        this.genetic = new BasicEA(pop, score);

        this.genetic.setSpeciation(new ArraySpeciation<DoubleArrayGenome>());

        genetic.addOperation(0.9, new Splice(PlantUniverse.GENOME_SIZE / 3));
        genetic.addOperation(0.1, new MutatePerturb(0.1));

        // Display

        this.universe = new PlantUniverse();
        this.universe.reset();


        DoubleArrayGenome bestGenome = (DoubleArrayGenome) genetic.getBestGenome();
        PlantPhysics physics = new PlantPhysics();
        PlantGrowth growth = new PlantGrowth();

        for (int i = 0; i < 100; i++) {
            physics.runPhysics(universe);
            growth.runGrowth(universe, bestGenome.getData());
        }

        this.display = new DisplayPlant();
        this.display.setUniverse(this.universe);
        this.getContentPane().add(this.display);

        setSize(PlantUniverse.UNIVERSE_WIDTH * 5, PlantUniverse.UNIVERSE_HEIGHT * 5);

        // start the background thread
        Thread t = new Thread(this);
        t.start();
    }

    public static void main(String[] args) {
        Milestone3Main t = new Milestone3Main();
        t.setVisible(true);
    }

    /**
     * Perform the training iterations/generations.
     */
    @Override
    public void run() {
        int generation = 0;
        for (; ; ) {
            generation++;
            this.genetic.iteration();

            this.universe.reset();

            DoubleArrayGenome bestGenome = (DoubleArrayGenome) this.genetic.getBestGenome();
            PlantGrowth growth = new PlantGrowth();
            PlantPhysics physics = new PlantPhysics();

            for (int i = 0; i < PlantUniverse.EVALUATION_CYCLES; i++) {
                physics.runPhysics(universe);
                growth.runGrowth(universe, bestGenome.getData());
            }

            this.display.setGeneration(generation);
            this.display.setBestScore(this.genetic.getBestGenome().getScore());
            this.display.repaint();

            System.out.println(Arrays.toString(bestGenome.getLongTermMemory()));

        }
    }
}
