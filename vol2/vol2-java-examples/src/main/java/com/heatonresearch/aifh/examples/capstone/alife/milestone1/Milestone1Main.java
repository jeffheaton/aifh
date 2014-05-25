package com.heatonresearch.aifh.examples.capstone.alife.milestone1;

import javax.swing.*;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 5/16/14
 * Time: 6:10 AM
 * To change this template use File | Settings | File Templates.
 */
public class Milestone1Main extends JFrame {

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
