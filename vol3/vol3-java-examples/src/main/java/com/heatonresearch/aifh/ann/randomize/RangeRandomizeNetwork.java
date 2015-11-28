package com.heatonresearch.aifh.ann.randomize;


import com.heatonresearch.aifh.ann.BasicNetwork;

public class RangeRandomizeNetwork extends AbstractRandomizeNetwork {

    private final double low;
    private final double high;

    public RangeRandomizeNetwork(double theLow, double theHigh) {
        this.low = theLow;
        this.high = theHigh;
    }

    public RangeRandomizeNetwork() {
        this(-1,1);
    }

    @Override
    public void randomize(BasicNetwork network) {
        for(int i=0;i<network.getWeights().length;i++) {
            network.getWeights()[i] = this.getRnd().nextDouble(low,high);
        }
    }


}
