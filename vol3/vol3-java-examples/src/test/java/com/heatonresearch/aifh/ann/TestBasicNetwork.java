package com.heatonresearch.aifh.ann;

import com.heatonresearch.aifh.AIFH;
import com.heatonresearch.aifh.AIFHError;
import com.heatonresearch.aifh.ann.activation.ActivationReLU;
import com.heatonresearch.aifh.ann.activation.ActivationSigmoid;
import com.heatonresearch.aifh.ann.activation.ActivationSoftMax;
import com.heatonresearch.aifh.ann.randomize.RangeRandomizeNetwork;
import com.heatonresearch.aifh.randomize.LinearCongruentialRandom;
import org.junit.Assert;
import org.junit.Test;

public class TestBasicNetwork {
    public static BasicNetwork buildSimpleXOR() {
        BasicNetwork network = new BasicNetwork();
        network.addLayer(new BasicLayer(null,true,2));
        network.addLayer(new BasicLayer(new ActivationSigmoid(),true,3));
        network.addLayer(new BasicLayer(new ActivationSigmoid(),false,1));
        network.finalizeStructure();

        RangeRandomizeNetwork randomize = new RangeRandomizeNetwork();
        randomize.setRnd(new LinearCongruentialRandom(10));
        randomize.randomize(network);
        return network;
    }

    @Test
    public void testWeightAccess() {
        BasicNetwork network = TestBasicNetwork.buildSimpleXOR();

        // layer 0
        Assert.assertEquals(-0.48463710059519793,network.getWeight(0,0,0), AIFH.DEFAULT_PRECISION);
        Assert.assertEquals(0.8420570357334933,network.getWeight(0,0,1), AIFH.DEFAULT_PRECISION);
        Assert.assertEquals(-0.5286518143323836,network.getWeight(0,0,2), AIFH.DEFAULT_PRECISION);

        Assert.assertEquals(-0.9318070094873679,network.getWeight(0,1,0), AIFH.DEFAULT_PRECISION);
        Assert.assertEquals(-0.38185835475610996,network.getWeight(0,1,1), AIFH.DEFAULT_PRECISION);
        Assert.assertEquals(-0.5337936066868234,network.getWeight(0,1,2), AIFH.DEFAULT_PRECISION);

        Assert.assertEquals(-0.38468537605011033,network.getWeight(0,2,0), AIFH.DEFAULT_PRECISION);
        Assert.assertEquals(-0.06280032523507262,network.getWeight(0,2,1), AIFH.DEFAULT_PRECISION);
        Assert.assertEquals(-0.7822212804533125,network.getWeight(0,2,2), AIFH.DEFAULT_PRECISION);

        // layer 1
        Assert.assertEquals(0.13864904435785697,network.getWeight(1,0,0), AIFH.DEFAULT_PRECISION);
        Assert.assertEquals(-0.1389734035215744,network.getWeight(1,1,0), AIFH.DEFAULT_PRECISION);
        Assert.assertEquals(0.34318625259753,network.getWeight(1,2,0), AIFH.DEFAULT_PRECISION);
    }

    @Test
    public void testWeightRange() {
        BasicNetwork network = TestBasicNetwork.buildSimpleXOR();

        // Layer too big
        try {
            network.getWeight(2, 0, 0);
            Assert.assertTrue(false);
        } catch(AIFHError ex) {
            Assert.assertNotNull(ex);
        }

        // From neuron too big
        try {
            network.getWeight(0, 3, 0);
            Assert.assertTrue(false);
        } catch(AIFHError ex) {
            Assert.assertNotNull(ex);
        }

        // To neuron too big
        try {
            network.getWeight(0, 0, 4);
            Assert.assertTrue(false);
        } catch(AIFHError ex) {
            Assert.assertNotNull(ex);
        }
    }

    @Test
    public void testCalculate() {
        BasicNetwork network = TestBasicNetwork.buildSimpleXOR();
        double[] out1 = network.computeRegression(new double[] {0.0, 0.0});
        Assert.assertEquals(0.34688637738116557, out1[0], AIFH.DEFAULT_PRECISION);
        double[] out2 = network.computeRegression(new double[] {1.0, 0.0});
        Assert.assertEquals(0.32943376685512565, out2[0], AIFH.DEFAULT_PRECISION);
        Assert.assertEquals(1,out2.length);
    }

    @Test
    public void testNeuronStructure() {
        Layer inputLayer,hidden1Layer,hidden2Layer,hidden3Layer,outputLayer;
        BasicNetwork network = new BasicNetwork();
        network.addLayer(inputLayer = new BasicLayer(null,true,90));
        network.addLayer(hidden1Layer = new BasicLayer(new ActivationReLU(),true,256));
        network.addLayer(hidden2Layer = new BasicLayer(new ActivationReLU(),true,128));
        network.addLayer(hidden3Layer = new BasicLayer(new ActivationReLU(),true,64));
        network.addLayer(outputLayer = new BasicLayer(new ActivationSoftMax(),false,10));
        network.finalizeStructure();

        Assert.assertEquals(91, inputLayer.getTotalCount());
        Assert.assertEquals(257, hidden1Layer.getTotalCount());
        Assert.assertEquals(129, hidden2Layer.getTotalCount());
        Assert.assertEquals(65, hidden3Layer.getTotalCount());
        Assert.assertEquals(10, outputLayer.getTotalCount());

        Assert.assertEquals(0, outputLayer.getNeuronIndex());
        Assert.assertEquals(10, hidden3Layer.getNeuronIndex());
        Assert.assertEquals(75, hidden2Layer.getNeuronIndex());
        Assert.assertEquals(204, hidden1Layer.getNeuronIndex());
        Assert.assertEquals(0, outputLayer.getNeuronIndex());

    }
}
