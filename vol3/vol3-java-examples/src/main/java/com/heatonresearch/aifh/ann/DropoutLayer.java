package com.heatonresearch.aifh.ann;

import com.heatonresearch.aifh.ann.activation.ActivationFunction;
import com.heatonresearch.aifh.randomize.GenerateRandom;

public class DropoutLayer extends BasicLayer {

    private double dropoutProbability;
    private boolean[] active;

    public DropoutLayer(final ActivationFunction theActivation, boolean theHasBias, int theCount, double theDropout) {
        super(theActivation,theHasBias,theCount);
        this.dropoutProbability = theDropout;
        this.active = new boolean[theCount];
        for(int i=0;i<this.active.length;i++) {
            this.active[i] = true;
        }
    }

    @Override
    public void trainingBatch(GenerateRandom rnd) {
        for(int i=0;i<this.active.length;i++) {
            this.active[i] = rnd.nextDouble()>this.dropoutProbability;
        }
    }

    public double getDropoutProbability() {
        return dropoutProbability;
    }

    public void setDropoutProbability(double dropoutProbability) {
        this.dropoutProbability = dropoutProbability;
    }

    public boolean[] getActive() {
        return active;
    }

    @Override
    public boolean isActive(int i) {
        if( getOwner().isNetworkTraining() ) {
            if (i<this.active.length) {
                return this.active[i];
            } else {
                return true;// bias always active.
            }
        } else {
            return true;
        }
    }
}
