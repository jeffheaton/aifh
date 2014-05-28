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
package com.heatonresearch.aifh.examples.swarm.flock;

import com.heatonresearch.aifh.distance.CalculateDistance;
import com.heatonresearch.aifh.distance.EuclideanDistance;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * This example plots a flock of particles that exhibit flocking behavior.  This is governed by the following three
 * simple rules:
 * <p/>
 * 1. Separation - avoid crowding neighbors (short range repulsion)
 * 2. Alignment - steer towards average heading of neighbors
 * 3. Cohesion - steer towards average position of neighbors (long range attraction)
 * <p/>
 * References:
 * <p/>
 * http://en.wikipedia.org/wiki/Flocking_(behavior)
 */
public class Flock2dWindow extends JFrame implements Runnable, ComponentListener, WindowListener {

    /**
     * The number of particles.
     */
    private final int PARTICLE_COUNT = 100;

    /**
     * The size of each particle.
     */
    private final double PARTICLE_SIZE = 10;

    /**
     * The particles.
     */
    private final List<Particle> particles;

    /**
     * An off-screen image to render to.
     */
    private Graphics offscreenGraphics;

    /**
     * The off screen image.
     */
    private BufferedImage offscreenImage;

    /**
     * Distance calculation.
     */
    private CalculateDistance distanceCalc = new EuclideanDistance();

    /**
     * The constant for cohesion.
     */
    private double constCohesion = 0.01;

    /**
     * The constant for alignment.
     */
    private double constAlignment = 0.5;

    /**
     * The constant for separation.
     */
    private double constSeparation = 0.25;

    /**
     * The constructor.
     */
    public Flock2dWindow() {
        setTitle("Flocking in 2D");
        setSize(1024, 768);
        this.particles = new ArrayList<Particle>();

        for (int i = 0; i < PARTICLE_COUNT; i++) {
            Particle p = new Particle(2);
            p.getLocation()[0] = Math.random() * this.getWidth();
            p.getLocation()[1] = Math.random() * this.getHeight();
            p.getVelocity()[0] = 3;
            p.getVelocity()[1] = Math.random() * 2.0 * Math.PI;
            this.particles.add(p);
        }

        // register for events
        this.addWindowListener(this);
        this.addComponentListener(this);
    }

    /**
     * Main entry point.
     *
     * @param args Not used.
     */
    public static void main(String[] args) {
        Flock2dWindow app = new Flock2dWindow();
        app.setVisible(true);
    }

    /**
     * Find the index that has the max value in a vector.
     *
     * @param data The vector.
     * @return The index.
     */
    public static int maxIndex(double[] data) {
        int result = -1;
        for (int i = 0; i < data.length; i++) {
            if (result == -1 || data[i] > data[result]) {
                result = i;
            }
        }
        return result;
    }

    /**
     * Get the mean particle location for the specified dimension.
     *
     * @param particles The particles.
     * @param dimension The dimension.
     * @return The mean.
     */
    public static double particleLocationMean(Collection<Particle> particles, int dimension) {
        double sum = 0;
        int count = 0;
        for (Particle p : particles) {
            sum += p.getLocation()[dimension];
            count++;
        }
        return sum / count;
    }

    /**
     * Get the particle velocity mean for the specified dimension.
     *
     * @param particles The particles.
     * @param dimension The dimension.
     * @return The velocity mean.
     */
    public static double particleVelocityMean(Collection<Particle> particles, int dimension) {
        double sum = 0;
        int count = 0;
        for (Particle p : particles) {
            sum += p.getVelocity()[dimension];
            count++;
        }
        return sum / count;
    }

    /**
     * Find the nearest neighbor particle.
     *
     * @param target    The particle to look for neighbors to.
     * @param particles All particles.
     * @param k         The number of particles to find.
     * @param maxDist   The max distance to check.
     * @return The nearest neighbors.
     */
    private Collection<Particle> findNearest(Particle target, Collection<Particle> particles, int k, double maxDist) {
        List<Particle> result = new ArrayList<Particle>();
        double[] tempDist = new double[k];
        int worstIndex = -1;

        for (Particle particle : particles) {
            if (particle == target) {
                continue;
            }
            double d = this.distanceCalc.calculate(particle.getLocation(), target.getLocation());

            if (d > maxDist) {
                continue;
            }

            if (result.size() < k) {
                tempDist[result.size()] = d;
                result.add(particle);
                worstIndex = maxIndex(tempDist);
            } else if (d < tempDist[worstIndex]) {
                tempDist[worstIndex] = d;
                result.set(worstIndex, particle);
                worstIndex = maxIndex(tempDist);
            }
        }

        return result;
    }

    /**
     * Perform the flocking.
     */
    private void flock() {
        for (Particle particle : this.particles) {
            ///////////////////////////////////////////////////////////////
            // Begin implementation of three very basic laws of flocking.
            ///////////////////////////////////////////////////////////////
            Collection<Particle> neighbors = findNearest(particle, this.particles, 5, Double.POSITIVE_INFINITY);
            Collection<Particle> nearest = findNearest(particle, this.particles, 5, 10);

            // 1. Separation - avoid crowding neighbors (short range repulsion)
            double separation = 0;
            if (nearest.size() > 0) {
                double meanX = particleLocationMean(nearest, 0);
                double meanY = particleLocationMean(nearest, 1);
                double dx = meanX - particle.getLocation()[0];
                double dy = meanY - particle.getLocation()[1];
                separation = Math.atan2(dx, dy) - particle.getVelocity()[1];
                separation += Math.PI;
            }

            // 2. Alignment - steer towards average heading of neighbors
            double alignment = 0;

            if (neighbors.size() > 0) {
                alignment = particleVelocityMean(neighbors, 1) - particle.getVelocity()[1];
            }

            // 3. Cohesion - steer towards average position of neighbors (long range attraction)
            double cohesion = 0;

            if (neighbors.size() > 0) {
                double meanX = particleLocationMean(this.particles, 0);
                double meanY = particleLocationMean(this.particles, 1);
                double dx = meanX - particle.getLocation()[0];
                double dy = meanY - particle.getLocation()[1];
                cohesion = Math.atan2(dx, dy) - particle.getVelocity()[1];
            }

            // perform the turn
            // The degree to which each of the three laws is applied is configurable.
            // The three default ratios that I provide work well.
            double turnAmount = (cohesion * this.constCohesion) + (alignment * this.constAlignment) + (separation * this.constSeparation);

            particle.getVelocity()[1] += turnAmount;

            ///////////////////////////////////////////////////////////////
            // End implementation of three very basic laws of flocking.
            ///////////////////////////////////////////////////////////////
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void run() {

        // create offscreen drawing buffer
        this.offscreenImage = new BufferedImage(getWidth(), getHeight(),
                BufferedImage.TYPE_INT_ARGB);

        this.offscreenGraphics = this.offscreenImage.createGraphics();

        for (; ; ) {
            // clear the off screen area
            this.offscreenGraphics.setColor(Color.black);
            this.offscreenGraphics.fillRect(0, 0, getWidth(), getHeight());

            // render the particles
            int[] x = new int[3];
            int[] y = new int[3];

            this.offscreenGraphics.setColor(Color.white);
            for (Particle p : this.particles) {
                x[0] = (int) p.getLocation()[0];
                y[0] = (int) p.getLocation()[1];

                double r = p.getVelocity()[1] + (Math.PI * 5.0) / 12.0;
                x[1] = x[0] - (int) (Math.cos(r) * PARTICLE_SIZE);
                y[1] = y[0] - (int) (Math.sin(r) * PARTICLE_SIZE);

                double r2 = p.getVelocity()[1] + (Math.PI * 7.0) / 12.0;
                x[2] = x[0] - (int) (Math.cos(r2) * PARTICLE_SIZE);
                y[2] = y[0] - (int) (Math.sin(r2) * PARTICLE_SIZE);

                this.offscreenGraphics.drawPolygon(x, y, 3);
                //this.offscreenGraphics.fillOval((int)p.getLocation()[0],(int)p.getLocation()[1],10,10);

                // move the particle
                double dx = Math.cos(r);
                double dy = Math.sin(r);
                p.getLocation()[0] += (dx * p.getVelocity()[0]);
                p.getLocation()[1] += (dy * p.getVelocity()[0]);

                // handle wraps
                if (p.getLocation()[0] < 0) {
                    p.getLocation()[0] = getWidth();
                }
                if (p.getLocation()[1] < 0) {
                    p.getLocation()[1] = getHeight();
                }
                if (p.getLocation()[0] > getWidth()) {
                    p.getLocation()[0] = 0;
                }
                if (p.getLocation()[1] > getHeight()) {
                    p.getLocation()[1] = 0;
                }

            }
            flock();

            try {
                Thread.sleep(10);
            } catch (InterruptedException e) {
                e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
            }

            // update the screen
            Graphics g = this.getGraphics();
            g.drawImage(this.offscreenImage, 0, 0, this);
        }

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void componentResized(final ComponentEvent e) {
        // create off-screen drawing area
        this.offscreenImage = new BufferedImage(getWidth(), getHeight(),
                BufferedImage.TYPE_INT_ARGB);
        this.offscreenGraphics = this.offscreenImage.getGraphics();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void componentMoved(final ComponentEvent e) {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void componentShown(final ComponentEvent e) {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void componentHidden(final ComponentEvent e) {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void windowOpened(final WindowEvent e) {
        // start the thread
        Thread t = new Thread(this);
        t.start();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void windowClosing(final WindowEvent e) {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void windowClosed(final WindowEvent e) {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void windowIconified(final WindowEvent e) {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void windowDeiconified(final WindowEvent e) {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void windowActivated(final WindowEvent e) {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void windowDeactivated(final WindowEvent e) {
    }
}
