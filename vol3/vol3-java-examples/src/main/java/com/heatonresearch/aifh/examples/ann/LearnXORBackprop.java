package com.heatonresearch.aifh.examples.ann;

import com.heatonresearch.aifh.ann.BasicLayer;
import com.heatonresearch.aifh.ann.BasicNetwork;
import com.heatonresearch.aifh.ann.activation.ActivationLinear;
import com.heatonresearch.aifh.ann.activation.ActivationReLU;
import com.heatonresearch.aifh.ann.activation.ActivationSigmoid;
import com.heatonresearch.aifh.ann.train.BackPropagation;

import java.util.Arrays;

/**
 * Created by Jeff on 11/27/2015.
 */
public class LearnXORBackprop {
    /**
     * The input necessary for XOR.
     */
    public static double XOR_INPUT[][] = { { 0.0, 0.0 }, { 1.0, 0.0 },
            { 0.0, 1.0 }, { 1.0, 1.0 } };

    /**
     * The ideal data necessary for XOR.
     */
    public static double XOR_IDEAL[][] = { { 0.0 }, { 1.0 }, { 1.0 }, { 0.0 } };

    /**
     * The main method.
     * @param args No arguments are used.
     */
    public static void main(final String args[]) {

        // create a neural network, without using a factory
        BasicNetwork network = new BasicNetwork();
        network.addLayer(new BasicLayer(null,true,2));
        network.addLayer(new BasicLayer(new ActivationSigmoid(),true,5));
        network.addLayer(new BasicLayer(new ActivationSigmoid(),false,1));
        network.finalizeStructure();
        network.reset();

        // train the neural network
        final BackPropagation train = new BackPropagation(network, XOR_INPUT, XOR_IDEAL, 0.7, 0.9);

        int epoch = 1;

        do {
            train.iteration();
            System.out.println("Epoch #" + epoch + " Error:" + train.getError());
            epoch++;
        } while(train.getError() > 0.01);

        // test the neural network
        System.out.println("Neural Network Results:");
        for(int i=0;i < XOR_INPUT.length; i++ ) {
            double[] output = network.compute(XOR_INPUT[i]);
            System.out.println(Arrays.toString(XOR_INPUT[i])
                    + ", actual=" + Arrays.toString(output)
                    + ",ideal=" + Arrays.toString(XOR_IDEAL[i]));
        }
    }
}
