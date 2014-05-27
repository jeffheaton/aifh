package com.heatonresearch.aifh.examples.capstone.alife.milestone1;

import javax.swing.*;

/**
 * This is the first milestone for the plants artificial life capstone.
 *
 * For this milestone, we would like to just display a "seed".  The seed is the physical starting
 * point for a plant.  Physics imposes limits on seed growth.  The plant genome provides instructions
 * for growth.
 */
public class Milestone1Main extends JFrame {

    /**
     * The plant display panel.
     */
    private DisplayPlant display;
    private PlantUniverse universe;

    public Milestone1Main() {
        this.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);

        this.universe = new PlantUniverse();
        this.universe.reset();
        this.display = new DisplayPlant();
        this.display.setUniverse(this.universe);
        this.getContentPane().add(this.display);

        setSize(PlantUniverse.UNIVERSE_WIDTH * 5, PlantUniverse.UNIVERSE_HEIGHT * 5);
    }

    public static void main(String[] args) {
        Milestone1Main t = new Milestone1Main();
        t.setVisible(true);
    }
}
