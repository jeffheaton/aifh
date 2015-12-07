package com.heatonresearch.aifh.ann;

import com.heatonresearch.aifh.ann.activation.ActivationFunction;
import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.sun.xml.internal.bind.annotation.OverrideAnnotationOf;

public class MaxPoolLayer implements Layer {

    public MaxPoolLayer(int[] count) {

    }


    @Override
    public int getCount() {
        return 0;
    }

    @Override
    public int getTotalCount() {
        return 0;
    }

    @Override
    public ActivationFunction getActivation() {
        return null;
    }

    @Override
    public void finalizeStructure(BasicNetwork theOwner, int theLayerIndex, int theNeuronIndex, int theWeightIndex) {

    }


    @Override
    public void computeLayer() {

    }

    @Override
    public int getWeightIndex() {
        return 0;
    }

    @Override
    public int getNeuronIndex() {
        return 0;
    }

    @Override
    public int getLayerIndex() {
        return 0;
    }

    @Override
    public void trainingBatch(GenerateRandom rnd) {

    }

    @Override
    public BasicNetwork getOwner() {
        return null;
    }

    @Override
    public boolean isActive(int i) {
        return false;
    }

    @Override
    public boolean hasBias() {
        return true;
    }
}
