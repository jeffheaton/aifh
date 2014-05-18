package com.heatonresearch.aifh.examples.capstone.model.milestone2;

import com.heatonresearch.aifh.general.data.BasicData;

import java.util.ArrayList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 5/17/14
 * Time: 2:44 PM
 * To change this template use File | Settings | File Templates.
 */
public class CrossValidateFold {
    private final List<BasicData> trainingSet = new ArrayList<BasicData>();
    private final List<BasicData> validationSet = new ArrayList<BasicData>();
    private double score;

    public List<BasicData> getTrainingSet() {
        return trainingSet;
    }

    public List<BasicData> getValidationSet() {
        return validationSet;
    }

    public double getScore() {
        return score;
    }

    public void setScore(double d) {
        this.score = d;
    }
}
