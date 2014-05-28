package com.heatonresearch.aifh.examples.swarm.flock;

/**
 * A particle.
 */
public class Particle {
    /**
     * The location.
     */
    private double[] location;
    /**
     * The velocity.
     */
    private double[] velocity;

    /**
     * The constructor.
     * @param dimensions The dimensions.
     */
    public Particle(int dimensions) {
        this.location = new double[dimensions];
        this.velocity = new double[dimensions];
    }

    /**
     * @return The location vector.
     */
    public double[] getLocation() {
        return this.location;
    }

    /**
     * @return The velocity vector.
     */
    public double[] getVelocity() {
        return this.velocity;
    }
}
