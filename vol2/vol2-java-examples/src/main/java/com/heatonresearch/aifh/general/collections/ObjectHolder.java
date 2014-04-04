package com.heatonresearch.aifh.general.collections;

import java.io.Serializable;

/**
 * Holds an object of the specified type.  Used with the ObjectChooser.
 * Associates an object with a probability.
 *
 * @param <T> The type of object to hold.
 */
public class ObjectHolder<T> implements Serializable {
    /**
     * The object to hold.
     */
    private final T obj;

    /**
     * The probability.
     */
    private final double probability;

    public ObjectHolder(T theObj, double probability) {
        this.obj = theObj;
        this.probability = probability;
    }

    /**
     * @return the opp
     */
    public T getObj() {
        return this.obj;
    }

    /**
     * @return the probability
     */
    public double getProbability() {
        return probability;
    }
}
