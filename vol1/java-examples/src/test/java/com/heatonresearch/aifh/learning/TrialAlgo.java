package com.heatonresearch.aifh.learning;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 8/31/13
 * Time: 11:54 AM
 * To change this template use File | Settings | File Templates.
 */
public class TrialAlgo implements MachineLearningAlgorithm {

    private final double[] memory = new double[3];

    @Override
    public double[] getLongTermMemory() {
        return memory;  //To change body of implemented methods use File | Settings | File Templates.
    }
}
