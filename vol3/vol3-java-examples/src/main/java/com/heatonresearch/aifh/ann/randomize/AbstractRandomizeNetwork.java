package com.heatonresearch.aifh.ann.randomize;

import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

/**
 * Created by jeff on 11/28/15.
 */
public abstract class AbstractRandomizeNetwork implements RandomizeNetwork {

    private GenerateRandom rnd = new MersenneTwisterGenerateRandom();

    public GenerateRandom getRnd() {
        return rnd;
    }

    public void setRnd(GenerateRandom rnd) {
        this.rnd = rnd;
    }
}
