/*
 * Artificial Intelligence for Humans
 * Volume 3: Deep Learning and Neural Networks
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh
 *
 * Copyright 2014-2015 by Jeff Heaton
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * For more information on Heaton Research copyrights, licenses
 * and trademarks visit:
 * http://www.heatonresearch.com/copyright
 */
package com.heatonresearch.aifh.examples.ann;

import com.heatonresearch.aifh.ann.BasicLayer;
import com.heatonresearch.aifh.ann.BasicNetwork;
import com.heatonresearch.aifh.ann.activation.ActivationSigmoid;
import com.heatonresearch.aifh.ann.train.ResilientPropagation;
import com.heatonresearch.aifh.general.data.BasicData;

import java.util.Arrays;
import java.util.List;


public class LearnXORRPROP {
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

        BasicNetwork network = new BasicNetwork();
        network.addLayer(new BasicLayer(null,true,2));
        network.addLayer(new BasicLayer(new ActivationSigmoid(),true,5));
        network.addLayer(new BasicLayer(new ActivationSigmoid(),false,1));
        network.finalizeStructure();
        network.reset();

        List<BasicData> trainingData = BasicData.combineXY(XOR_INPUT, XOR_IDEAL);

        // train the neural network
        final ResilientPropagation train = new ResilientPropagation(network, trainingData);

        int epoch = 1;

        do {
            train.iteration();
            System.out.println("Epoch #" + epoch + " Error:" + train.getLastError());
            epoch++;
        } while(train.getLastError() > 0.01);

        // test the neural network
        System.out.println("Neural Network Results:");
        for(int i=0;i < XOR_INPUT.length; i++ ) {
            double[] output = network.computeRegression(XOR_INPUT[i]);
            System.out.println(Arrays.toString(XOR_INPUT[i])
                    + ", actual=" + Arrays.toString(output)
                    + ",ideal=" + Arrays.toString(XOR_IDEAL[i]));
        }
    }
}

