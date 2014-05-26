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
public class Milestone2Main extends JFrame implements Runnable {
    private DisplayPlant display;
    private PlantUniverse universe;
    private static final double[] SAMPLE_PLANT = {0.14571628630139233, 0.7197846628513596, 0.7231784466511191, 0.5323520114051941, 0.598996879435396, 0.34274504759458013, 0.8578776058517235, 0.6044269107777535, 0.23325200014592307, 0.9641721808631352, 0.18395062640547966, 0.025616301916038697, 0.29862907359377233, 0.7250040575067441, 0.5866401251651715, 0.6048653467891864};


    public Milestone2Main() {
        this.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);

        this.universe = new PlantUniverse();
        this.universe.reset();

        MersenneTwisterGenerateRandom rnd = new MersenneTwisterGenerateRandom();

        this.display = new DisplayPlant();
        this.display.setUniverse(this.universe);
        this.getContentPane().add(this.display);

        setSize(PlantUniverse.UNIVERSE_WIDTH * 5, PlantUniverse.UNIVERSE_HEIGHT * 5);

        Thread t = new Thread(this);
        t.start();
    }

    public static void main(String[] args) {
        Milestone2Main t = new Milestone2Main();
        t.setVisible(true);
    }

    @Override
    public void run() {
        PlantPhysics physics = new PlantPhysics();
        PlantGrowth growth = new PlantGrowth();

        for(int i=0;i<100;i++) {
            physics.runPhysics(universe);
            growth.runGrowth(universe,SAMPLE_PLANT);
            this.display.repaint();
            try {
                Thread.sleep(100);
            } catch (InterruptedException e) {
                e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
            }
        }
    }
}
