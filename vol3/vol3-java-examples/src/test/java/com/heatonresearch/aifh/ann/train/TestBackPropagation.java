package com.heatonresearch.aifh.ann.train;

import com.heatonresearch.aifh.ann.BasicNetwork;
import com.heatonresearch.aifh.ann.TestBasicNetwork;
import com.heatonresearch.aifh.general.data.BasicData;
import org.junit.Assert;
import org.junit.Test;

import java.util.List;

public class TestBackPropagation {

    /**
     * The input necessary for XOR.
     */
    public static double XOR_INPUT[][] = { { 0.0, 0.0 }, { 1.0, 0.0 },
            { 0.0, 1.0 }, { 1.0, 1.0 } };

    /**
     * The ideal data necessary for XOR.
     */
    public static double XOR_IDEAL[][] = { { 0.0 }, { 1.0 }, { 1.0 }, { 0.0 } };


    public static void testXOR(BasicNetwork network, BackPropagation train, int maxEpoch) {
        int epoch = 1;

        do {
            train.iteration();
            epoch++;
        } while(train.getLastError() > 0.01 && epoch<(maxEpoch+10));
        Assert.assertEquals(epoch,maxEpoch);
    }


    @Test
    public void testNesterov() {
        BasicNetwork network = TestBasicNetwork.buildSimpleXOR();

        List<BasicData> trainingData = BasicData.combineXY(XOR_INPUT, XOR_IDEAL);

        // train the neural network
        final BackPropagation train = new BackPropagation(network, trainingData, 0.7, 0.9);

        TestBackPropagation.testXOR(network,train,63);


    }
}
