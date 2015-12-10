package com.heatonresearch.aifh.ann;

import com.heatonresearch.aifh.ann.activation.ActivationFunction;
import com.heatonresearch.aifh.ann.train.GradientCalc;
import com.heatonresearch.aifh.randomize.GenerateRandom;

public interface Layer {
    int getCount();
    int getTotalCount();
    ActivationFunction getActivation();
    void finalizeStructure(BasicNetwork theOwner, int theLayerIndex,
                           TempStructureCounts counts);

    void computeLayer();

    void computeGradient(GradientCalc calc);

    int getWeightIndex();

    int getNeuronIndex();

    int getLayerIndexReverse();

    int getLayerIndex();

    void trainingBatch(GenerateRandom rnd);

    BasicNetwork getOwner();

    boolean isActive(int i);

    boolean hasBias();

    int[] getDimensionCounts();

    int getWeightDepthUnit();
    int getNeuronDepthUnit();

}
