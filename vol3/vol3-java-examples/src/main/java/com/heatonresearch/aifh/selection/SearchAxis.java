package com.heatonresearch.aifh.selection;

public interface SearchAxis {
    void reset();
    boolean advance();
    Object currentState();
}
