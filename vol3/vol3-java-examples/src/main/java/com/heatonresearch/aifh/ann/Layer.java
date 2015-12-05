package com.heatonresearch.aifh.ann;

import com.heatonresearch.aifh.ann.activation.ActivationFunction;
import com.heatonresearch.aifh.randomize.GenerateRandom;

public interface Layer {
    int getCount();
    int getTotalCount();
    ActivationFunction getActivation();
    void finalizeStructure(BasicNetwork theOwner, int theLayerIndex,
                           int theNeuronIndex, int theWeightIndex);

    void computeLayer();

    int getWeightIndex();

    int getNeuronIndex();

    int getLayerIndex();

    void trainingBatch(GenerateRandom rnd);

    BasicNetwork getOwner();

    boolean isActive(int i);

}
