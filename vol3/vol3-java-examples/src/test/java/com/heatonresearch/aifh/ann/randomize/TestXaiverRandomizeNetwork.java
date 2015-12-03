package com.heatonresearch.aifh.ann.randomize;

import com.heatonresearch.aifh.AIFH;
import com.heatonresearch.aifh.ann.BasicLayer;
import com.heatonresearch.aifh.ann.BasicNetwork;
import com.heatonresearch.aifh.ann.activation.ActivationSigmoid;
import com.heatonresearch.aifh.randomize.LinearCongruentialRandom;
import org.junit.Assert;
import org.junit.Test;

import java.util.Arrays;

public class TestXaiverRandomizeNetwork {
    @Test
    public void testRandomize() {
        BasicNetwork network = new BasicNetwork();
        network.addLayer(new BasicLayer(null,true,2));
        network.addLayer(new BasicLayer(new ActivationSigmoid(),true,3));
        network.addLayer(new BasicLayer(new ActivationSigmoid(),false,1));
        network.finalizeStructure();

        XaiverRandomizeNetwork randomize = new XaiverRandomizeNetwork();
        randomize.setRnd(new LinearCongruentialRandom(10));
        randomize.randomize(network);

        double[] expected = {-0.48063016461488856, -0.6268874420756946, 3.2098992119169534E-4, -0.1914041699971798, 1.040596563646283, -0.485186340462527, -1.110171460956511, -1.0430309621862426, -0.13325874823843634, -0.18257850835630843, 0.228311697697274, 0.2916967306617774, -0.43452699284274987};

        Assert.assertArrayEquals(expected,network.getWeights(), AIFH.DEFAULT_PRECISION);

    }
}
