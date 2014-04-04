package com.heatonresearch.aifh.general.collections;

import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.RandomChoice;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * This class is used to choose between several objects with a specified probability.
 * @param <T> The type of object to choose from.
 */
public class ChooseObject<T> implements Serializable {
    /**
     * The objects that we are choosing from.
     */
    private final List<ObjectHolder<T>> list = new ArrayList<ObjectHolder<T>>();

    /**
     * The random choose.
     */
    private RandomChoice chooser;

    /**
     * Finalize the structure and set the probabilities.
     */
    public void finalizeStructure() {
        double[] d = new double[size()];
        for(int i=0;i<size();i++) {
            d[i] = list.get(i).getProbability();
        }

        this.chooser = new RandomChoice(d);
    }

    /**
     * Add an object.
     * @param probability The probability to choose this object.
     * @param opp The object to add.
     */
    public void add(double probability, T opp) {
        list.add(new ObjectHolder<T>(opp,probability));
    }

    /**
     * @return The number of objects added.
     */
    public int size() {
        return list.size();
    }

    /**
     * Choose a random object.
     * @param theGenerator
     * @return The random choice.
     */
    public T pick(GenerateRandom theGenerator) {
        int index = this.chooser.generate(theGenerator);
        return this.list.get(index).getObj();
    }

    /**
     * @return The object to choose from.
     */
    public List<ObjectHolder<T>> getList() {
        return this.list;
    }

    /**
     * CLear all objects from the collection.
     */
    public void clear() {
        this.list.clear();
    }

    /**
     * @return The first object in the list.
     */
    public T pickFirst() {
        return this.list.get(0).getObj();
    }
}
