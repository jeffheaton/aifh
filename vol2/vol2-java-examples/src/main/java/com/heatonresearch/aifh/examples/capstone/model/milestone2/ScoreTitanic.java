package com.heatonresearch.aifh.examples.capstone.model.milestone2;

import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.learning.ClassificationAlgorithm;
import com.heatonresearch.aifh.learning.MLMethod;
import com.heatonresearch.aifh.learning.RegressionAlgorithm;
import com.heatonresearch.aifh.learning.score.ScoreFunction;

import java.util.List;

/**
 * Score the Titanic model. The score is percentage cases predicted correctly.
 */
public class ScoreTitanic implements ScoreFunction {

    /**
     * The training data.
     */
    private final List<BasicData> trainingData;

    /**
     * Construct the score function.
     *
     * @param theTrainingData The training data.
     */
    public ScoreTitanic(final List<BasicData> theTrainingData) {
        this.trainingData = theTrainingData;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double calculateScore(final MLMethod algo) {
        int incorrectCount = 0;
        int totalCount = 0;

        final RegressionAlgorithm alg = (RegressionAlgorithm) algo;

        for (final BasicData aTrainingData : this.trainingData) {
            totalCount++;
            boolean predictSurvive = alg.computeRegression(aTrainingData.getInput())[0]>0.5;
            boolean idealSurvive = aTrainingData.getIdeal()[0]>0.5;

            if (predictSurvive==idealSurvive) {
                incorrectCount++;
            }
        }

        return (double) incorrectCount / (double) totalCount;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean shouldMinimize() {
        return false;
    }
}
