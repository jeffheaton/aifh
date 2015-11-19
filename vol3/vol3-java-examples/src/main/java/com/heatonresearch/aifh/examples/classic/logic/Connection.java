package com.heatonresearch.aifh.examples.classic.logic;

/**
 * Connection between two neurons.
 */
public class Connection {
    /**
     * The weight of the connection.
     */
    private double weight;

    /**
     * The parent/source.
     */
    private Neuron parent;

    /**
     * Create a connection.
     * @param weight The weight.
     * @param parent The parent/source neuron.
     */
    public Connection(double weight, Neuron parent) {
        this.weight = weight;
        this.parent = parent;
    }

    /**
     * @return The weight.
     */
    public double getWeight() {
        return weight;
    }

    /**
     * Set the weight.
     * @param weight The weight.
     */
    public void setWeight(double weight) {
        this.weight = weight;
    }

    /**
     * @return The parent neuron.
     */
    public Neuron getParent() {
        return parent;
    }

    /**
     * Set the parent neuron.
     * @param parent The parent neuron.
     */
    public void setParent(Neuron parent) {
        this.parent = parent;
    }
}
