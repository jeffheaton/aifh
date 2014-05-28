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
package com.heatonresearch.aifh.examples.capstone.alife.milestone2;

import com.heatonresearch.aifh.examples.capstone.alife.milestone1.DisplayPlant;
import com.heatonresearch.aifh.examples.capstone.alife.milestone1.PlantUniverse;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

import javax.swing.*;

/**
 * The second milestone produces an animation, where we see a plant follow a genome's plan and grow a seed.
 * A sample plant is provided.  The numbers for the sample plant's genome were actually produced using
 * evolution (milestone 3).  However this anachronistic information does provide you with a way to check
 * your growth and physics algorithms.
 */
public class Milestone2Main extends JFrame implements Runnable {

    /**
     * Display the plant.
     */
    private DisplayPlant display;

    /**
     * The universe the plant lives in.
     */
    private PlantUniverse universe;

    /**
     * A sample plant that we will animate.
     */
    private static final double[] SAMPLE_PLANT = {0.08414097456375995, 0.11845586131703176, 0.1868971940834313, 0.4346911204161327,
            0.024190631402031804, 0.5773526701833149, 0.8997253827355136, 0.9267311086327318, 0.04639229538493471, 0.8190692654645835,
            0.06531672676605614, 0.026431639742068264, 0.31497914852215286, 1.0276526539348398, 0.03303133293309127, 0.35946010922382937};

    /**
     * Constructor.
     */
    public Milestone2Main() {
        this.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);

        this.universe = new PlantUniverse();
        this.universe.reset();

        this.display = new DisplayPlant();
        this.display.setUniverse(this.universe);
        this.getContentPane().add(this.display);

        setSize(PlantUniverse.UNIVERSE_WIDTH * 5, PlantUniverse.UNIVERSE_HEIGHT * 5);

        Thread t = new Thread(this);
        t.start();
    }

    /**
     * Start the program.
     *
     * @param args Not used.
     */
    public static void main(String[] args) {
        Milestone2Main t = new Milestone2Main();
        t.setVisible(true);
    }

    /**
     * Perform one frame of animation.
     */
    @Override
    public void run() {
        PlantPhysics physics = new PlantPhysics();
        PlantGrowth growth = new PlantGrowth();

        for (int i = 0; i < PlantUniverse.EVALUATION_CYCLES; i++) {
            physics.runPhysics(universe);
            growth.runGrowth(universe, SAMPLE_PLANT);
            this.display.repaint();
            try {
                Thread.sleep(100);
            } catch (InterruptedException e) {
                e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
            }
        }
    }
}
