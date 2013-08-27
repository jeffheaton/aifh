package com.heatonresearch.aifh.learning.score;

import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.learning.ClassificationAlgorithm;
import com.heatonresearch.aifh.learning.MachineLearningAlgorithm;

import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 7/30/13
 * Time: 3:48 PM
 * To change this template use File | Settings | File Templates.
 */
public class ScoreClassificationData implements ScoreFunction {

    private final List<BasicData> trainingData;

    public ScoreClassificationData(List<BasicData> theTrainingData) {
        this.trainingData = theTrainingData;
    }

    @Override
    public double calculateScore(final MachineLearningAlgorithm algo) {
        int incorrectCount = 0;
        int totalCount = 0;

        ClassificationAlgorithm ralgo = (ClassificationAlgorithm) algo;

        for (int row = 0; row < this.trainingData.size(); row++) {
            totalCount++;
            BasicData pair = this.trainingData.get(row);
            int output = ralgo.computeClassification(pair.getInput());

            if (output != this.trainingData.get(row).getIdeal()[0]) {
                incorrectCount++;
            }
        }

        return (double) incorrectCount / (double) totalCount;
    }
}
