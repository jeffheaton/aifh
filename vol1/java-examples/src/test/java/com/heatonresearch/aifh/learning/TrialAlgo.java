package com.heatonresearch.aifh.learning;

/**
 * A trial algorithm.
 */
public class TrialAlgo implements MachineLearningAlgorithm {

    private final double[] memory = new double[3];

    @Override
    public double[] getLongTermMemory() {
        return memory;  //To change body of implemented methods use File | Settings | File Templates.
    }
}
