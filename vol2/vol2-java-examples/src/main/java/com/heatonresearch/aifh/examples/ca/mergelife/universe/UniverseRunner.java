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
package com.heatonresearch.aifh.examples.ca.mergelife.universe;

import com.heatonresearch.aifh.examples.ca.mergelife.physics.Physics;
import com.heatonresearch.aifh.randomize.GenerateRandom;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Used to run a universe's physics and provide display.
 */
public class UniverseRunner {

    /**
     * The event used to sync waiting for tasks to stop.
     */
    private final Lock accessLock = new ReentrantLock();

    /**
     * Should we automatically kill a universe that stabilizes.
     */
    private boolean autoKill;

    /**
     * The difference between two frames.
     */
    private double diff;

    /**
     * The current iteration.
     */
    private int iteration;

    /**
     * The physics calculator.
     */
    private final Physics physics;

    /**
     * The universe that is used to generate the next frame.
     */
    private final Universe tempUniverse;

    /**
     * The universe.
     */
    private final Universe universe;

    /**
     * The constructor.
     *
     * @param theUniverse The universe.
     * @param thePhysics  The physics calculator.
     */
    public UniverseRunner(final Universe theUniverse, final Physics thePhysics) {
        this.universe = theUniverse;
        this.tempUniverse = (Universe) theUniverse.clone();
        this.physics = thePhysics;
    }

    /**
     * Advance one frame.
     */
    public void advance(GenerateRandom rnd) {
        final int height = this.universe.getHeight();
        final int width = this.universe.getWidth();

        try {
            // Copy the current universe to the temp universe.
            // The next frame is rendered into the temp universe.
            this.accessLock.lock();
            this.tempUniverse.copy(this.universe);

            // Run each pixel through the physics calculator.
            for (int col = 0; col < width; col++) {
                for (int row = 0; row < height; row++) {
                    this.physics.processPixel(this.tempUniverse, row, col);
                }
            }

            this.diff = this.tempUniverse.compare(this.universe);

            this.iteration++;

            // Copy the temp universe back into
            this.universe.copy(this.tempUniverse);

            if (this.diff < 0.0001 && this.iteration > 5) {
                if (this.autoKill) {
                    reset(rnd);
                }
            }
        } finally {
            this.accessLock.unlock();
        }
    }

    /**
     * Perform a genetic crossover between two parent universes.  A new universe will be created with attributes
     * from the two parents.
     *
     * @param rnd              Random number generator.
     * @param crossoverParent1 The first parent.
     * @param crossoverParent2 The second parent.
     */
    public void crossover(final GenerateRandom rnd, final UniverseRunner crossoverParent1,
                          final UniverseRunner crossoverParent2) {
        final double[] parent1 = crossoverParent1.getPhysics().getData();
        final double[] parent2 = crossoverParent2.getPhysics().getData();
        final double[] child = getPhysics().getData();
        final int len = parent1.length;
        final int p1 = (int) (rnd.nextDouble() * (double) len);
        final int p2 = (int) (rnd.nextDouble() * (double) len);

        for (int i = 0; i < getPhysics().getData().length; i++) {
            if (i < p1) {
                child[i] = parent1[i];
            } else if (i >= p1 && i <= p2) {
                child[i] = parent2[i];
            } else if (i > p2) {
                child[i] = parent1[i];
            }
        }
    }

    /**
     * @return The difference between the last two frames.
     */
    public double getDiff() {
        return this.diff;
    }

    /**
     * @return The total number of iterations.
     */
    public int getIterations() {
        return this.iteration;
    }

    /**
     * @return The physics calculator.
     */
    public Physics getPhysics() {
        return this.physics;
    }

    /**
     * @return the autoKill
     */
    public boolean isAutoKill() {
        return this.autoKill;
    }

    /**
     * Perform a mutate on a parent and generate a new child.  The parent is not changed.
     *
     * @param rnd           Random number generator.
     * @param sourcePhysics The parent object.
     * @param probChange    The probability of changing an individual element.
     * @param perturb       The amount that an object is changed by.
     */
    public void mutate(final GenerateRandom rnd,
                       final Physics sourcePhysics, final double probChange,
                       final double perturb) {
        getPhysics().copyData(sourcePhysics.getData());

        for (int i = 0; i < sourcePhysics.getData().length; i++) {
            if (rnd.nextDouble() < probChange) {
                getPhysics().getData()[i] += perturb * rnd.nextDouble(-1, 1);
            }
        }

    }

    /**
     * Randomize the universe grid.
     *
     * @param rnd Random number generator.
     */
    public void randomize(GenerateRandom rnd) {
        this.accessLock.lock();
        this.universe.randomize(rnd);
        this.iteration = 0;
        this.accessLock.unlock();
    }

    /**
     * Randomize the universe grid and the physics calculator.
     *
     * @param rnd A random number generator.
     */
    public void reset(GenerateRandom rnd) {
        this.accessLock.lock();
        this.physics.randomize();
        this.universe.randomize(rnd);
        this.iteration = 0;
        this.accessLock.unlock();
    }


    /**
     * @param autoKill the autoKill to set
     */
    public void setAutoKill(final boolean autoKill) {
        this.autoKill = autoKill;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        return "Iteration: " + this.iteration + ", Diff=" + this.diff;
    }

}
