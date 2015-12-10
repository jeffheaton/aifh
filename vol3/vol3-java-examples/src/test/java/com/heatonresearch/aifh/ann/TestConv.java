package com.heatonresearch.aifh.ann;

import com.heatonresearch.aifh.AIFH;
import com.heatonresearch.aifh.ann.activation.ActivationReLU;
import com.heatonresearch.aifh.ann.activation.ActivationSigmoid;
import com.heatonresearch.aifh.ann.activation.ActivationSoftMax;
import com.heatonresearch.aifh.ann.randomize.RangeRandomizeNetwork;
import com.heatonresearch.aifh.randomize.LinearCongruentialRandom;
import org.junit.Assert;
import org.junit.Test;

/**
 * Created by jeff on 12/8/15.
 */
public class TestConv {
    public static BasicNetwork buildSimpleConv() {
        BasicNetwork network = new BasicNetwork();
        int[] inputShape = new int[] {8,8,3};
        network.addLayer(new BasicLayer(new ActivationReLU(),true,inputShape));
        network.addLayer(new Conv2DLayer(new ActivationReLU(),2,2,2));
        network.addLayer(new BasicLayer(new ActivationSoftMax(),false,2));
        network.finalizeStructure();

        RangeRandomizeNetwork randomize = new RangeRandomizeNetwork();
        randomize.setRnd(new LinearCongruentialRandom(10));
        randomize.randomize(network);
        return network;
    }

    @Test
    public void testConvCounts() {
        BasicNetwork network = buildSimpleConv();
        Assert.assertEquals(1562,network.getWeights().length);
        Assert.assertEquals(1562,network.getLayers().get(0).getWeightIndex());
        Assert.assertEquals(18,network.getLayers().get(1).getWeightIndex());
        Assert.assertEquals(0,network.getLayers().get(2).getWeightIndex());
    }

    @Test
    public void testOutput() {
        BasicNetwork network = buildSimpleConv();
        double[] input = new double[8*8*3];
        double[] output = network.computeRegression(input);
        Assert.assertEquals(2, output.length);
        Assert.assertArrayEquals(new double[] {
                0.50772252335148,
                0.49227747664851984},output, AIFH.DEFAULT_PRECISION);
    }
}
