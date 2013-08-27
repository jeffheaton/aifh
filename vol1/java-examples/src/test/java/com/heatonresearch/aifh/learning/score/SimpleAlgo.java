package com.heatonresearch.aifh.learning.score;

import com.heatonresearch.aifh.learning.ClassificationAlgorithm;
import com.heatonresearch.aifh.learning.RegressionAlgorithm;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 8/27/13
 * Time: 6:44 AM
 * To change this template use File | Settings | File Templates.
 */
public class SimpleAlgo implements RegressionAlgorithm, ClassificationAlgorithm {
    private double[] cannedResults;
    private int currentIndex = 0;

    public SimpleAlgo(double[] theCannedResults) {
        this.cannedResults = theCannedResults;
    }

    @Override
    public int computeClassification(final double[] input) {
        return (int) this.cannedResults[this.currentIndex++];
    }

    @Override
    public double[] computeRegression(final double[] input) {
        double[] result = new double[1];
        result[0] = this.cannedResults[currentIndex++];
        return result;
    }

    @Override
    public double[] getLongTermMemory() {
        return new double[0];  //To change body of implemented methods use File | Settings | File Templates.
    }
}
