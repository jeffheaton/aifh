package com.heatonresearch.aifh.selection;

import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

public class RandomModelSelection extends ModelSelection {

    private GenerateRandom rnd = new MersenneTwisterGenerateRandom();

    @Override
    public Object[] next() {
        Object[] result = new Object[getSearchAxises().size()];

        for(int i=0;i<result.length;i++) {
            result[i] = getSearchAxises().get(i).sample(this.rnd);
        }

        return result;
    }

    public GenerateRandom getRandom() {
        return rnd;
    }

    public void setRandom(GenerateRandom rnd) {
        this.rnd = rnd;
    }
}
