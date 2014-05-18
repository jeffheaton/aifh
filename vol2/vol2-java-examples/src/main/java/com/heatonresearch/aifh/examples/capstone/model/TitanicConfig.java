package com.heatonresearch.aifh.examples.capstone.model;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 5/18/14
 * Time: 7:27 AM
 * To change this template use File | Settings | File Templates.
 */
public class TitanicConfig {
    public static final String TrainingFilename = "train.csv";
    public static final String TestFilename = "test.csv";
    public static final String NormDumpFilename = "normalized_dump.csv";
    public static final int InputFeatureCount = 13;

    public static final double InputNormalizeLow = -1;
    public static final double InputNormalizeHigh = 1;

    public static final double PredictSurvive = 1;
    public static final double PredictPerish = 0;

    public static final int FoldCount = 5;
    public static final int ParticleCount = 30;
    public static final int RBF_COUNT = 5;
    public static final int AllowNoImprovement = 100;

}
