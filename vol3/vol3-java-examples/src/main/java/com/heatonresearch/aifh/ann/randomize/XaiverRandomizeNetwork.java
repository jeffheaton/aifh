package com.heatonresearch.aifh.ann.randomize;

import com.heatonresearch.aifh.ann.BasicNetwork;

/**
 * Created by jeff on 11/28/15.
 */
public class XaiverRandomizeNetwork extends AbstractRandomizeNetwork {

    private void randomizeLayer(BasicNetwork network, int fromLayer) {
        final int fromCount = network.getLayerTotalNeuronCount(fromLayer);
        final int toCount = network.getLayerNeuronCount(fromLayer + 1);

        for (int fromNeuron = 0; fromNeuron < fromCount; fromNeuron++) {
            for (int toNeuron = 0; toNeuron < toCount; toNeuron++) {
                double sigma = Math.sqrt(2.0/(fromCount+toCount));
                double w = this.getRnd().nextGaussian() * sigma;
                network.setWeight(fromLayer, fromNeuron, toNeuron, w);
            }
        }
    }

    @Override
    public void randomize(BasicNetwork network) {
        for (int i = 0; i < network.getLayers().size() - 1; i++) {
            randomizeLayer(network, i);
        }
    }
}
