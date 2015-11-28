package com.heatonresearch.aifh.ann.train;

import com.heatonresearch.aifh.ann.BasicNetwork;
import com.heatonresearch.aifh.ann.train.error.CrossEntropyErrorFunction;
import com.heatonresearch.aifh.error.ErrorCalculation;
import com.heatonresearch.aifh.error.ErrorCalculationMSE;

/**
 * Created by Jeff on 11/27/2015.
 */
public class BackPropagation implements GradientCalcOwner {

    private final BasicNetwork network;
    private final double[][] x;
    private final double[][] y;
    private final double learningRate;
    private final double momentum;
    private final int batchSize = 0;
    private final GradientCalc gradients;
    private final double[] lastDelta;
    private ErrorCalculation errorCalc = new ErrorCalculationMSE();
    private double currentError = 1.0;

    public BackPropagation(BasicNetwork theNetwork, double[][] theX, double[][] theY, double theLearningRate, double theMomentum) {
        this.network = theNetwork;
        this.x = theX;
        this.y = theY;
        this.learningRate = theLearningRate;
        this.momentum = theMomentum;
        this.gradients = new GradientCalc(this.network, new CrossEntropyErrorFunction(), this);
        this.lastDelta = new double[theNetwork.getWeights().length];
    }

    public void iteration() {
        this.gradients.reset();
        this.errorCalc.clear();
        for(int i=0;i<this.x.length;i++) {
            this.gradients.process(errorCalc, x[i],y[i]);
        }
        this.currentError = this.errorCalc.calculate();

        for(int i=0;i<this.network.getWeights().length;i++) {
            double delta = (this.gradients.getGradients()[i] * this.learningRate) + (this.lastDelta[i]*this.momentum);
            this.network.getWeights()[i] += delta;
            this.lastDelta[i] = delta;
        }

    }

    public double getError() {
     return this.currentError;
    }

    /**
     * @return How much to apply l1 regularization penalty, 0 (default) for none.
     */
    @Override
    public double getL1() {
        return 0;
    }

    /**
     * @return How much to apply l2 regularization penalty, 0 (default) for none.
     */
    @Override
    public double getL2() {
        return 0;
    }
}
