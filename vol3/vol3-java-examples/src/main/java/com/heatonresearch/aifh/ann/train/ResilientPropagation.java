package com.heatonresearch.aifh.ann.train;

import com.heatonresearch.aifh.ann.BasicNetwork;
import com.heatonresearch.aifh.ann.train.error.CrossEntropyErrorFunction;
import com.heatonresearch.aifh.error.ErrorCalculation;
import com.heatonresearch.aifh.error.ErrorCalculationMSE;
import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.learning.LearningMethod;
import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

import java.util.List;

public class ResilientPropagation implements GradientCalcOwner, LearningMethod {

    private final BasicNetwork network;
    private final List<BasicData> training;


    private final GradientCalc gradients;
    private final double[] lastDelta;
    private final double[] lastGradients;
    private ErrorCalculation errorCalc = new ErrorCalculationMSE();
    private double currentError = 1.0;
    private double l1;
    private double l2;
    private double[] updateValues;

    /**
     * The POSITIVE ETA value. This is specified by the resilient propagation
     * algorithm. This is the percentage by which the deltas are increased by if
     * the partial derivative is greater than zero.
     */
    public static final double POSITIVE_ETA = 1.2;

    /**
     * The NEGATIVE ETA value. This is specified by the resilient propagation
     * algorithm. This is the percentage by which the deltas are increased by if
     * the partial derivative is less than zero.
     */
    public static final double NEGATIVE_ETA = 0.5;

    /**
     * The minimum delta value for a weight matrix value.
     */
    public static final double DELTA_MIN = 1e-6;

    /**
     * The starting update for a delta.
     */
    public static final double DEFAULT_INITIAL_UPDATE = 0.1;

    /**
     * The maximum amount a delta can reach.
     */
    public static final double DEFAULT_MAX_STEP = 50;

    public ResilientPropagation(BasicNetwork theNetwork, List<BasicData> theTraining) {
        this.network = theNetwork;
        this.training = theTraining;
        this.gradients = new GradientCalc(this.network, new CrossEntropyErrorFunction(), this);
        this.lastDelta = new double[theNetwork.getWeights().length];
        this.updateValues = new double[theNetwork.getWeights().length];
        this.lastGradients = new double[theNetwork.getWeights().length];

        for (int i = 0; i < this.updateValues.length; i++) {
            this.updateValues[i] = ResilientPropagation.DEFAULT_INITIAL_UPDATE;
        }
    }

    public void iteration() {
        this.gradients.reset();
        this.errorCalc.clear();

        // Calculate gradients for entire training set, RPROP does not do online.
        for(int i=0;i<this.training.size();i++) {
            BasicData element = this.training.get(i);
            this.gradients.process(errorCalc, element.getInput(), element.getIdeal());
        }
        this.currentError = this.errorCalc.calculate();

        // Apply the gradients according to the RPROP algorithm.
        for(int i=0;i<this.gradients.getGradients().length;i++) {
            double delta = calculateWeightDelta(this.gradients.getGradients(), this.lastGradients, i);
            this.lastGradients[i] = this.gradients.getGradients()[i];
            this.lastDelta[i] = delta;
            this.network.getWeights()[i]+=delta;
        }


    }

    public double calculateWeightDelta(final double[] gradients,
                                   final double[] lastGradient, final int index) {
        // multiply the current and previous gradient, and take the
        // sign. We want to see if the gradient has changed its sign.
        final int change = (int)Math.signum(gradients[index] * lastGradient[index]);
        double weightChange = 0;

        // if the gradient has retained its sign, then we increase the
        // delta so that it will converge faster
        if (change > 0) {
            double delta = this.updateValues[index]
                    * ResilientPropagation.POSITIVE_ETA;
            delta = Math.min(delta, ResilientPropagation.DEFAULT_MAX_STEP);
            weightChange = -Math.signum(gradients[index]) * delta;
            this.updateValues[index] = delta;
            lastGradient[index] = gradients[index];
        } else if (change < 0) {
            // if change<0, then the sign has changed, and the last
            // delta was too big
            double delta = this.updateValues[index]
                    * ResilientPropagation.NEGATIVE_ETA;
            delta = Math.max(delta, ResilientPropagation.DELTA_MIN);
            this.updateValues[index] = delta;
            weightChange = -this.lastDelta[index];
            // set the previous gradent to zero so that there will be no
            // adjustment the next iteration
            lastGradient[index] = 0;
        } else if (change == 0) {
            // if change==0 then there is no change to the delta
            final double delta = this.updateValues[index];
            weightChange = -Math.signum(gradients[index]) * delta;
            lastGradient[index] = gradients[index];
        }

        // apply the weight change, if any
        return weightChange;
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
