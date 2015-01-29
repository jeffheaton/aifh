package com.heatonresearch.aifh.examples.classic.hopfield;

import com.heatonresearch.aifh.energetic.HopfieldTankNetwork;
import com.heatonresearch.aifh.learning.MLMethod;
import com.heatonresearch.aifh.learning.score.ScoreFunction;

/**
 * Created with IntelliJ IDEA.
 * User: Jeff
 * Date: 1/27/15
 * Time: 6:27 PM
 * To change this template use File | Settings | File Templates.
 */
public class ObjectiveFunctionTSP implements ScoreFunction {

    private HopfieldOptimizeTSP owner;

    public ObjectiveFunctionTSP(HopfieldOptimizeTSP hopfieldOptimizeTSP) {
        this.owner = hopfieldOptimizeTSP;
    }

    @Override
    public double calculateScore(MLMethod algo) {
        HopfieldTankNetwork hop = (HopfieldTankNetwork)algo;
        hop.runUntilStable(1000);
        return this.owner.calculateEnergy(hop.getCurrentState());
    }

    @Override
    public boolean shouldMinimize() {
        return true;
    }
}
