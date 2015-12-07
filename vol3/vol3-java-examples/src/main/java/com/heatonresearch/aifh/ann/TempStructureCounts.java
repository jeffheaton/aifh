package com.heatonresearch.aifh.ann;

/**
 * Created by jeff on 12/7/15.
 */
public class TempStructureCounts {
    int neuronCount = 0;
    int weightCount = 0;

    public int getNeuronCount() {
        return this.neuronCount;
    }

    public int getWeightCount() {
        return this.weightCount;
    }

    public void addNeuronCount(int i) {
        this.neuronCount+=i;
    }

    public void addWeightCount(int i) {
        this.weightCount+=i;
    }
}
