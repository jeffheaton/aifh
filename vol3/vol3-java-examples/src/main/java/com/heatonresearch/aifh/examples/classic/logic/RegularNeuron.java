package com.heatonresearch.aifh.examples.classic.logic;

import java.util.ArrayList;
import java.util.List;

/**
 * A regular neuron, can function as either a hidden our output.
 */
public class RegularNeuron implements Neuron {
    /**
     * The parents to this neuron.
     */
    private final List<Connection> parents = new ArrayList<>();

    /**
     * The bias.
     */
    private double bias;

    /**
     * Construct the neuron.
     * @param bias The neuron's bias.
     */
    public RegularNeuron(double bias) {
        this.bias = bias;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double compute() {
        double sum = this.bias;
        for(Connection c : this.parents) {
            sum += c.getWeight() * c.getParent().compute();
        }

        if(sum>=0.5) {
            return 1;
        } else {
            return 0;
        }
    }

    /**
     * @return The parent neurons.
     */
    public List<Connection> getParents() {
        return parents;
    }
}
