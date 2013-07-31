package com.heatonresearch.aifh.learning.score;

import com.heatonresearch.aifh.error.ErrorCalculation;
import com.heatonresearch.aifh.error.ErrorCalculationMSE;
import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.learning.MachineLearningAlgorithm;
import com.heatonresearch.aifh.learning.RegressionAlgorithm;

import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 7/30/13
 * Time: 2:13 PM
 * To change this template use File | Settings | File Templates.
 */
public class ScoreRegressionData implements ScoreFunction {

    private ErrorCalculation errorCalc = new ErrorCalculationMSE();
    private final List<BasicData> trainingData;

    public ScoreRegressionData(List<BasicData> theTrainingData) {
        this.trainingData = theTrainingData;
    }

    @Override
    public double calculateScore(MachineLearningAlgorithm algo) {
        RegressionAlgorithm ralgo = (RegressionAlgorithm) algo;
        // evaulate
        errorCalc.clear();
        for (int row = 0; row < this.trainingData.size(); row++) {
            BasicData pair = this.trainingData.get(row);
            double[] output = ralgo.computeRegression(pair.getInput());
            errorCalc.updateError(output, pair.getIdeal(), 1.0);
        }

        return errorCalc.calculate();
    }

    public ErrorCalculation getErrorCalc() {
        return errorCalc;
    }

    public void setErrorCalc(final ErrorCalculation errorCalc) {
        this.errorCalc = errorCalc;
    }

    public List<BasicData> getTrainingData() {
        return trainingData;
    }
}
