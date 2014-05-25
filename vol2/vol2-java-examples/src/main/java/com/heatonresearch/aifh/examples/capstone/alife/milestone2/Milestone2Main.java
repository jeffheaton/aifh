package com.heatonresearch.aifh.examples.capstone.alife.milestone2;

import com.heatonresearch.aifh.examples.capstone.alife.milestone1.DisplayPlant;
import com.heatonresearch.aifh.examples.capstone.alife.milestone1.PlantUniverse;
import com.heatonresearch.aifh.randomize.MersenneTwisterFactory;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

import javax.swing.*;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 5/23/14
 * Time: 9:09 AM
 * To change this template use File | Settings | File Templates.
 */
public class Milestone2Main extends JFrame {
    private DisplayPlant display;
    private PlantUniverse universe;

    public Milestone2Main() {
        this.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);

        this.universe = new PlantUniverse();
        this.universe.reset();

        double[] genome = new double[PlantUniverse.CELL_VECTOR_LENGTH*4];
        MersenneTwisterGenerateRandom rnd = new MersenneTwisterGenerateRandom();

        for(int i=0;i<genome.length;i++) {
            genome[i] = rnd.nextDouble(0,1);
        }

        PlantPhysics physics = new PlantPhysics();
        PlantGrowth growth = new PlantGrowth();

        for(int i=0;i<100;i++) {
            physics.runPhysics(universe);
            growth.runGrowth(universe,genome);
        }

        this.display = new DisplayPlant();
        this.display.setUniverse(this.universe);
        this.getContentPane().add(this.display);

        setSize(PlantUniverse.UNIVERSE_WIDTH * 5, PlantUniverse.UNIVERSE_HEIGHT * 5);
    }

    public static void main(String[] args) {
        Milestone2Main t = new Milestone2Main();
        t.setVisible(true);
    }
}
