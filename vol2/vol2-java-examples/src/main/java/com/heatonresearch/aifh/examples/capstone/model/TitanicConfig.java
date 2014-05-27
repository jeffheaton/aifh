package com.heatonresearch.aifh.examples.capstone.model;

/**
 * Configuration data for the Titanic project.
 */
public class TitanicConfig {

    /**
     * The name of the training data. (that we are to train on)
     */
    public static final String TrainingFilename = "train.csv";

    /**
     * The name of the test data. (that Kaggle evaluates us on)
     */
    public static final String TestFilename = "test.csv";

    /**
     * Dump the normalized data to this file.  This file is not actually used, but rather can be viewed to see
     * the normalization.
     */
    public static final String NormDumpFilename = "normalized_dump.csv";

    /**
     * The number of input features used.
     */
    public static final int InputFeatureCount = 13;

    /**
     * The low range of the normalization.
     */
    public static final double InputNormalizeLow = -1;

    /**
     * The high range of the normalization.
     */
    public static final double InputNormalizeHigh = 1;

    /**
     * The value used for a prediction of survival.
     */
    public static final double PredictSurvive = 1;

    /**
     * The value used for a prediction of perish.
     */
    public static final double PredictPerish = 0;

    /**
     * The number of folds to use.
     */
    public static final int FoldCount = 5;

    /**
     * The number of particles to use.
     */
    public static final int ParticleCount = 30;

    /**
     * The number of RBF functions to use in each network.
     */
    public static final int RBF_COUNT = 5;

    /**
     * The number of iterations to allow with no improvement.
     */
    public static final int AllowNoImprovement = 100;

}
