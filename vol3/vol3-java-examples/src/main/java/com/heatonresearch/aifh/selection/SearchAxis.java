package com.heatonresearch.aifh.selection;

import com.heatonresearch.aifh.randomize.GenerateRandom;

public interface SearchAxis {
    void reset();
    boolean advance();
    Object currentState();
    Object sample(GenerateRandom rnd);
}
