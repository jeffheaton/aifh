package com.heatonresearch.aifh.discrete;

import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 8/25/13
 * Time: 3:46 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class DiscreteAnneal {
    private GenerateRandom rnd = new MersenneTwisterGenerateRandom();
    private double globalBestScore = Double.POSITIVE_INFINITY;
    private double currentScore;
    private int kMax;
    private int k;
    private double startingTemperature;
    private double endingTemperature;
    private double currentTemperature;
    private int cycles = 100;
    private double lastProbability;
    private boolean shouldMinimize;

    public DiscreteAnneal(boolean theShouldMinimize, int theKMax, double theStartingTemperature, double theEndingTemperature) {
        this.kMax = theKMax;
        this.startingTemperature = theStartingTemperature;
        this.endingTemperature = theEndingTemperature;
        this.shouldMinimize = theShouldMinimize;
    }

    public double coolingSchedule() {
        double ex = (double) k / (double) kMax;
        return this.startingTemperature * Math.pow(this.endingTemperature / this.startingTemperature, ex);
    }

    public void iteration() {
        if (k == 0) {
            this.currentScore = evaluate();
            foundNewBest();
            this.globalBestScore = this.currentScore;
        }

        k++;

        this.currentTemperature = coolingSchedule();

        for (int cycle = 0; cycle < this.cycles; cycle++) {
            // backup current state
            backupState();

            // randomize the method
            moveToNeighbor();

            // did we improve it?  Only keep the new method if it improved (greedy).
            double trialScore = evaluate();

            // was this iteration an improvement?  If so, always keep.
            boolean keep = false;

            if ((trialScore < this.currentScore) ? shouldMinimize : !shouldMinimize) {
                // it was better, so always keep it
                keep = true;
            } else {
                // it was worse, so we might keep it
                this.lastProbability = calcProbability(currentScore, trialScore, this.currentTemperature);
                if (this.lastProbability > this.rnd.nextDouble()) {
                    keep = true;
                }
            }

            if (keep) {
                this.currentScore = trialScore;
                // better than global error
                if (trialScore < this.globalBestScore ? shouldMinimize : !shouldMinimize) {
                    this.globalBestScore = trialScore;
                    foundNewBest();
                }
            } else {
                restoreState();
            }
        }
    }

    public abstract void backupState();

    public abstract void restoreState();

    public abstract void foundNewBest();

    public abstract void moveToNeighbor();

    public abstract double evaluate();

    public boolean done() {
        return k >= kMax;
    }

    public double getBestScore() {
        return this.globalBestScore;
    }

    public double calcProbability(double ecurrent, double enew, double t) {
        return Math.exp(-(Math.abs(enew - ecurrent) / t));
    }

    public int getK() {
        return this.k;
    }

    public int getCycles() {
        return cycles;
    }

    public void setCycles(final int cycles) {
        this.cycles = cycles;
    }

    public String getStatus() {
        StringBuilder result = new StringBuilder();
        result.append("k=");
        result.append(this.k);
        result.append(",kMax=");
        result.append(this.kMax);
        result.append(",t=");
        result.append(this.currentTemperature);
        result.append(",prob=");
        result.append(this.lastProbability);
        return result.toString();
    }
}
