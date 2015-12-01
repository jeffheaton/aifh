package com.heatonresearch.aifh.ann;

import com.heatonresearch.aifh.ann.activation.ActivationFunction;

public interface Layer {
    int getCount();

    int getTotalCount();

    int getContextCount();

    ActivationFunction getActivation();

    Layer getContextFedBy();
}
