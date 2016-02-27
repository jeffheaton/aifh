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

import org.deeplearning4j.nn.api.OptimizationAlgorithm;
import org.deeplearning4j.nn.conf.MultiLayerConfiguration;
import org.deeplearning4j.nn.conf.NeuralNetConfiguration;
import org.deeplearning4j.nn.conf.Updater;
import org.deeplearning4j.nn.conf.layers.DenseLayer;
import org.deeplearning4j.nn.conf.layers.OutputLayer;
import org.deeplearning4j.nn.multilayer.MultiLayerNetwork;
import org.deeplearning4j.nn.weights.WeightInit;
import org.deeplearning4j.optimize.listeners.ScoreIterationListener;
import org.nd4j.linalg.api.ndarray.INDArray;
import org.nd4j.linalg.dataset.DataSet;
import org.nd4j.linalg.factory.Nd4j;
import org.nd4j.linalg.lossfunctions.LossFunctions.LossFunction;

/**
 * This is a VERY basic example of how to use DL4J to create a neural network to train a feed forward neural network
 * to emulate the XOR function.  Because the data for this operator is only 4 elements there is only a training set,
 * no validation set.  For a simple, yet more typical, example refer to the Iris example.
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
     * @param args Not used.
     */
    public static void main(String[] args) {
        int seed = 43;
        double learningRate = 0.4;
        int nEpochs = 100;

        int numInputs = XOR_INPUT[0].length;
        int numOutputs = XOR_IDEAL[0].length;
        int numHiddenNodes = 4;

        // Setup training data.
        INDArray xorInput = Nd4j.create(XOR_INPUT);
        INDArray xorIdeal = Nd4j.create(XOR_IDEAL);
        DataSet xorDataSet = new DataSet(xorInput,xorIdeal);

        // Create neural network.
        MultiLayerConfiguration conf = new NeuralNetConfiguration.Builder()
                .seed(seed)
                .iterations(1)
                .optimizationAlgo(OptimizationAlgorithm.STOCHASTIC_GRADIENT_DESCENT)
                .learningRate(learningRate)
                .updater(Updater.NESTEROVS).momentum(0.9)
                .list(2)
                .layer(0, new DenseLayer.Builder().nIn(numInputs).nOut(numHiddenNodes)
                        .weightInit(WeightInit.XAVIER)
                        .activation("relu")
                        .build())
                .layer(1, new OutputLayer.Builder(LossFunction.MSE)
                        .weightInit(WeightInit.XAVIER)
                        .activation("identity")
                        .nIn(numHiddenNodes).nOut(numOutputs).build())
                .pretrain(false).backprop(true).build();


        MultiLayerNetwork model = new MultiLayerNetwork(conf);
        model.init();
        model.setListeners(new ScoreIterationListener(1));


        // Train
        for ( int n = 0; n < nEpochs; n++) {
            model.fit( xorDataSet );
        }


        // Evaluate
        System.out.println("Evaluating neural network.");
        for(int i=0;i<4;i++) {
            INDArray input = xorInput.getRow(i);
            INDArray output = model.output(input);
            System.out.println( input + " : " + output);
        }
    }
}