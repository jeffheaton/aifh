package com.heatonresearch.aifh.examples.classic.logic;

/**
 * A very simple input neuron.
 */
public class InputNeuron implements Neuron {
    /**
     * The value of the input neuron.
     */
    private double value;

    /**
     * @return The current value of the input neuron.
     */
    public double getValue() {
        return value;
    }

    /**
     * Set the value of the input neuron.
     * @param value The new value of the input neuron.
     */
    public void setValue(double value) {
        this.value = value;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double compute() {
        return this.value;
    }
}
