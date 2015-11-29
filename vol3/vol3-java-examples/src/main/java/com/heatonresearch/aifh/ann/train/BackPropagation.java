package com.heatonresearch.aifh.ann.train;

import com.heatonresearch.aifh.ann.BasicNetwork;
import com.heatonresearch.aifh.ann.train.error.CrossEntropyErrorFunction;
import com.heatonresearch.aifh.error.ErrorCalculation;
import com.heatonresearch.aifh.error.ErrorCalculationMSE;
import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.learning.LearningMethod;

import java.util.List;

/**
 * Created by Jeff on 11/27/2015.
 */
public class BackPropagation implements GradientCalcOwner, LearningMethod {

    private final BasicNetwork network;
    private final List<BasicData> training;
    private final double learningRate;
    private final double momentum;
    private final int batchSize = 0;
    private final GradientCalc gradients;
    private final double[] lastDelta;
    private ErrorCalculation errorCalc = new ErrorCalculationMSE();
    private double currentError = 1.0;
    private double l1;
    private double l2;

    public BackPropagation(BasicNetwork theNetwork, List<BasicData> theTraining, double theLearningRate, double theMomentum) {
        this.network = theNetwork;
        this.training = theTraining;
        this.learningRate = theLearningRate;
        this.momentum = theMomentum;
        this.gradients = new GradientCalc(this.network, new CrossEntropyErrorFunction(), this);
        this.lastDelta = new double[theNetwork.getWeights().length];
    }

    public void iteration() {
        this.gradients.reset();
        this.errorCalc.clear();
        for(int i=0;i<this.training.size();i++) {
            BasicData element = this.training.get(i);
            this.gradients.process(errorCalc, element.getInput(), element.getIdeal());
        }
        this.currentError = this.errorCalc.calculate();

        for(int i=0;i<this.network.getWeights().length;i++) {
            double delta = (this.gradients.getGradients()[i] * this.learningRate) + (this.lastDelta[i]*this.momentum);
            this.network.getWeights()[i] += delta;
            this.lastDelta[i] = delta;
        }

    }

    /**
     * @return The error from the last training iteration.
     */
    @Override
    public double getLastError() {
        return this.currentError;
    }

    /**
     * @return True, if we are done learning.  Not all learning algorithms know when they are done, in this case
     * false is always returned.
     */
    @Override
    public boolean done() {
        return false;
    }

    /**
     * @return A string that indicates the status of training.
     */
    @Override
    public String getStatus() {
        return "";
    }

    /**
     * Should be called after the last iteration to make sure training completes any final tasks.
     */
    @Override
    public void finishTraining() {

    }

    /**
     * @return How much to apply l1 regularization penalty, 0 (default) for none.
     */
    @Override
    public double getL1() {
        return this.l1;
    }

    /**
     * @return How much to apply l2 regularization penalty, 0 (default) for none.
     */
    @Override
    public double getL2() {
        return this.l2;
    }

    public void setL1(double theL1) {
        this.l1 = theL1;
    }

    public void setL2(double theL2) {
        this.l2 = theL2;
    }
}
