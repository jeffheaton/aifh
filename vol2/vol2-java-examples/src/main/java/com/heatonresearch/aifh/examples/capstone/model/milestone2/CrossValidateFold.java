package com.heatonresearch.aifh.examples.capstone.model.milestone2;

import com.heatonresearch.aifh.general.data.BasicData;

import java.util.ArrayList;
import java.util.List;

/**
 * A cross validation fold.  This contains a training and validation set.  A score is also
 * held for the validation sets.
 */
public class CrossValidateFold {
    /**
     * The training set.
     */
    private final List<BasicData> trainingSet = new ArrayList<BasicData>();

    /**
     * The validation set.
     */
    private final List<BasicData> validationSet = new ArrayList<BasicData>();

    /**
     * The score.
     */
    private double score;

    /**
     * @return The training set.
     */
    public List<BasicData> getTrainingSet() {
        return trainingSet;
    }

    /**
     * @return The validation set.
     */
    public List<BasicData> getValidationSet() {
        return validationSet;
    }

    /**
     * @return The score.
     */
    public double getScore() {
        return score;
    }

    /**
     * Set the score.
     * @param d The score.
     */
    public void setScore(double d) {
        this.score = d;
    }
}
