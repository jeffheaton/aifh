package com.heatonresearch.aifh.learning;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 8/24/13
 * Time: 7:55 PM
 * To change this template use File | Settings | File Templates.
 */
public interface LearningAlgorithm {
    void iteration();

    double getLastError();

    boolean done();

    String getStatus();
}
