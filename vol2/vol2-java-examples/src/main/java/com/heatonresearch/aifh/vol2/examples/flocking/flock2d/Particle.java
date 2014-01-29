package com.heatonresearch.aifh.vol2.examples.flocking.flock2d;

public class Particle {
    private double[] location;
    private double[] velocity;

    public Particle(int dimensions) {
        this.location = new double[dimensions];
        this.velocity = new double[dimensions];
    }

    public double[] getLocation() {
        return this.location;
    }

    public double[] getVelocity() {
        return this.velocity;
    }
}
