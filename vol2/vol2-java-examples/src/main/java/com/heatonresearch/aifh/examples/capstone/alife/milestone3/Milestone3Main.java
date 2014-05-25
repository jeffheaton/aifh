package com.heatonresearch.aifh.examples.capstone.alife.milestone3;

import com.heatonresearch.aifh.evolutionary.population.BasicPopulation;
import com.heatonresearch.aifh.evolutionary.population.Population;
import com.heatonresearch.aifh.evolutionary.species.BasicSpecies;
import com.heatonresearch.aifh.evolutionary.train.EvolutionaryAlgorithm;
import com.heatonresearch.aifh.evolutionary.train.basic.BasicEA;
import com.heatonresearch.aifh.examples.capstone.alife.milestone1.DisplayPlant;
import com.heatonresearch.aifh.examples.capstone.alife.milestone1.PlantUniverse;
import com.heatonresearch.aifh.examples.capstone.alife.milestone2.PlantGrowth;
import com.heatonresearch.aifh.examples.capstone.alife.milestone2.PlantPhysics;
import com.heatonresearch.aifh.examples.ga.tsp.TSPScore;
import com.heatonresearch.aifh.genetic.crossover.Splice;
import com.heatonresearch.aifh.genetic.crossover.SpliceNoRepeat;
import com.heatonresearch.aifh.genetic.genome.DoubleArrayGenome;
import com.heatonresearch.aifh.genetic.genome.DoubleArrayGenomeFactory;
import com.heatonresearch.aifh.genetic.genome.IntegerArrayGenome;
import com.heatonresearch.aifh.genetic.genome.IntegerArrayGenomeFactory;
import com.heatonresearch.aifh.genetic.mutate.MutatePerturb;
import com.heatonresearch.aifh.genetic.mutate.MutateShuffle;
import com.heatonresearch.aifh.genetic.species.ArraySpeciation;
import com.heatonresearch.aifh.learning.score.ScoreFunction;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

import javax.swing.*;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 5/24/14
 * Time: 6:14 AM
 * To change this template use File | Settings | File Templates.
 */
public class Milestone3Main extends JFrame implements Runnable {
    private DisplayPlant display;
    private PlantUniverse universe;

    private Population pop;
    private PlantScore score;
    private BasicEA genetic;

    private MersenneTwisterGenerateRandom rnd = new MersenneTwisterGenerateRandom();

    private DoubleArrayGenome randomGenome() {
        DoubleArrayGenome genome = new DoubleArrayGenome(PlantUniverse.GENOME_SIZE);

        for(int i=0;i<genome.size();i++) {
            genome.getData()[i] = rnd.nextDouble(0,1);
        }
        return genome;
    }

    private Population initPopulation()
    {
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

    public Milestone3Main() {
        this.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);



        this.pop = initPopulation();
        this.score =  new PlantScore();
        this.genetic = new BasicEA(pop,score);

        this.genetic.setSpeciation(new ArraySpeciation<DoubleArrayGenome>());

        genetic.addOperation(0.9,new Splice(PlantUniverse.GENOME_SIZE/3));
        genetic.addOperation(0.1,new MutatePerturb(0.1));

        // Display

        this.universe = new PlantUniverse();
        this.universe.reset();


        DoubleArrayGenome bestGenome = (DoubleArrayGenome)genetic.getBestGenome();
        PlantPhysics physics = new PlantPhysics();
        PlantGrowth growth = new PlantGrowth();

        for(int i=0;i<100;i++) {
            physics.runPhysics(universe);
            growth.runGrowth(universe,bestGenome.getData());
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

    @Override
    public void run() {
        int generation = 0;
        for(;;) {
            generation++;
            this.genetic.iteration();

            this.universe.reset();

            DoubleArrayGenome bestGenome = (DoubleArrayGenome)this.genetic.getBestGenome();
            PlantGrowth growth = new PlantGrowth();
            PlantPhysics physics = new PlantPhysics();

            for(int i=0;i<100;i++) {
                physics.runPhysics(universe);
                growth.runGrowth(universe,bestGenome.getData());
            }

            this.display.setGeneration(generation);
            this.display.setBestScore(this.genetic.getBestGenome().getScore());
            this.display.repaint();

        }
    }
}
