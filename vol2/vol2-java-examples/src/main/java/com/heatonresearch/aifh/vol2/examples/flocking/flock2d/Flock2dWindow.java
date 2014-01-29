package com.heatonresearch.aifh.vol2.examples.flocking.flock2d;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.Collection;

//import com.heatonresearch.aifh.vol1.distance.*;

public class Flock2dWindow extends JFrame implements Runnable, ComponentListener, WindowListener {

    private final int PARTICLE_COUNT = 100;
    private final double PARTICLE_SIZE = 10;
    private final Particle[] particles;
    private Graphics offscreenGraphics;
    private Image offscreenImage;
    //private CalculateDistance distanceCalc = new EuclideanDistance();

    public Flock2dWindow() {
        setTitle("Flocking in 2D");
        setSize(1024,768);
        this.particles = new Particle[PARTICLE_COUNT];

        for(int i=0;i<this.particles.length;i++) {
            this.particles[i] = new Particle(2);
            this.particles[i].getLocation()[0] = Math.random() * this.getWidth();
            this.particles[i].getLocation()[1] = Math.random() * this.getHeight();
            this.particles[i].getVelocity()[0] = 1;
            this.particles[i].getVelocity()[1] = Math.random() * 2.0 * Math.PI;
        }

        // register for events
        this.addWindowListener(this);
        this.addComponentListener(this);
    }

    public static void main(String[] args) {
        Flock2dWindow app = new Flock2dWindow();
        app.setVisible(true);
    }

    private Collection<Particle> findNearest(Particle target, Particle[] particles, int k, double maxDist) {
        Collection<Particle> result = new ArrayList<Particle>();
        double[] tempDist = new double[k];
        int idx = 0;
        int worstIdx = -1;
        int dist, agent;

        for(Particle particle : particles) {
            if( particle==target ) {
                continue;
            }
            double d = 0;// this.distanceCalc.calculate(particle.getLocation(),target.getLocation());

            if( d>maxDist) {
                continue;
            }

            if( result.size()<k ) {
                tempDist[result.size()] = d;
                result.add(particle);
            } else {
                for(int i=0;i<result.size();i++) {
                    //if( )
                }
            }
        }

        return result;
    }

    /*private void flock() {
        ///////////////////////////////////////////////////////////////
        // Begin implementation of three very basic laws of flocking.
        ///////////////////////////////////////////////////////////////
        double targetAngle = 0;

        Particle[] neighbors = ENCOG.MathUtil.kNearest(this.agents[i], this.agents, 5, Number.MAX_VALUE, 0, 2);
        Particle[] nearest = ENCOG.MathUtil.kNearest(this.agents[i], this.agents, 5, 10, 0, 2);

        // 1. Separation - avoid crowding neighbors (short range repulsion)
        separation = 0;
        if (nearest.length > 0) {
            meanX = ENCOG.ArrayUtil.arrayMean(nearest, 0);
            meanY = ENCOG.ArrayUtil.arrayMean(nearest, 1);
            dx = meanX - this.agents[i][0];
            dy = meanY - this.agents[i][1];
            separation = (Math.atan2(dx, dy) * 180 / Math.PI) - this.agents[i][2];
            separation += 180;
        }

        // 2. Alignment - steer towards average heading of neighbors
        alignment = 0;

        if (neighbors.length > 0) {
            alignment = ENCOG.ArrayUtil.arrayMean(neighbors, 2) - this.agents[i][2];
        }

        if (this.callbackNeighbors !== null) {
            this.callbackNeighbors(i, neighbors);
        }

        // 3. Cohesion - steer towards average position of neighbors (long range attraction)
        cohesion = 0;

        if (neighbors.length > 0) {
            meanX = ENCOG.ArrayUtil.arrayMean(this.agents, 0);
            meanY = ENCOG.ArrayUtil.arrayMean(this.agents, 1);
            dx = meanX - this.agents[i][0];
            dy = meanY - this.agents[i][1];
            cohesion = (Math.atan2(dx, dy) * 180 / Math.PI) - this.agents[i][2];
        }

        // perform the turn
        // The degree to which each of the three laws is applied is configurable.
        // The three default ratios that I provide work well.
        turnAmount = (cohesion * this.constCohesion) + (alignment * this.constAlignment) + (separation * this.constSeparation);

        this.agents[i][2] += turnAmount;

        ///////////////////////////////////////////////////////////////
        // End implementation of three very basic laws of flocking.
        ///////////////////////////////////////////////////////////////
    }          */

    @Override
    public void run() {
        for(;;) {
        // clear the off screen area
        this.offscreenGraphics.setColor(Color.black);
        this.offscreenGraphics.fillRect(0,0,getWidth(),getHeight());

        // render the particles
        int[] x = new int[3];
        int[] y = new int[3];

        this.offscreenGraphics.setColor(Color.white);
        for(Particle p:this.particles) {
            x[0] = (int)p.getLocation()[0];
            y[0] = (int)p.getLocation()[1];

            double r = p.getVelocity()[1] + (Math.PI * 5.0) / 12.0;
            x[1] = x[0] - (int)(Math.cos(r) * PARTICLE_SIZE);
            y[1] = y[0] - (int)(Math.sin(r) * PARTICLE_SIZE);

            double r2 = p.getVelocity()[1] + (Math.PI * 7.0) / 12.0;
            x[2] = x[0] - (int)(Math.cos(r2) * PARTICLE_SIZE);
            y[2] = y[0] - (int)(Math.sin(r2) * PARTICLE_SIZE);

            this.offscreenGraphics.drawPolygon(x,y,3);
            //this.offscreenGraphics.fillOval((int)p.getLocation()[0],(int)p.getLocation()[1],10,10);

            // move the particle
            double dx = Math.cos(r);
            double dy = Math.sin(r);
            p.getLocation()[0] += (dx * p.getVelocity()[0]);
            p.getLocation()[1] += (dy * p.getVelocity()[0]);

            // handle wraps
            if( p.getLocation()[0]<0 ) {
                p.getLocation()[0] = getWidth();
            }
            if( p.getLocation()[1]<0 ) {
                p.getLocation()[1] = getHeight();
            }
            if( p.getLocation()[0]>getWidth() ) {
                p.getLocation()[0] = 0;
            }
            if( p.getLocation()[1]>getHeight() ) {
                p.getLocation()[1] = 0;
            }

        }
            try {
                Thread.sleep(10);
            } catch (InterruptedException e) {
                e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
            }

            // update the screen
        Graphics g = this.getGraphics();
        g.drawImage(this.offscreenImage,0,0,this);
        }

    }

    @Override
    public void componentResized(final ComponentEvent e) {
        // create off-screen drawing area
        this.offscreenImage = new BufferedImage(getWidth(), getHeight(),
                BufferedImage.TYPE_INT_ARGB);
        this.offscreenGraphics = this.offscreenImage.getGraphics();
    }

    @Override
    public void componentMoved(final ComponentEvent e) {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public void componentShown(final ComponentEvent e) {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public void componentHidden(final ComponentEvent e) {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public void windowOpened(final WindowEvent e) {
        // start the thread
        Thread t = new Thread(this);
        t.start();
    }

    @Override
    public void windowClosing(final WindowEvent e) {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public void windowClosed(final WindowEvent e) {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public void windowIconified(final WindowEvent e) {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public void windowDeiconified(final WindowEvent e) {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public void windowActivated(final WindowEvent e) {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public void windowDeactivated(final WindowEvent e) {
        //To change body of implemented methods use File | Settings | File Templates.
    }
}
