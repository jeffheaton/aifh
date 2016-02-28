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

import com.heatonresearch.aifh.util.MNIST;
import com.heatonresearch.aifh.util.MNISTReader;
import org.deeplearning4j.datasets.iterator.DataSetIterator;
import org.deeplearning4j.datasets.iterator.impl.ListDataSetIterator;
import org.deeplearning4j.earlystopping.EarlyStoppingConfiguration;
import org.deeplearning4j.earlystopping.EarlyStoppingModelSaver;
import org.deeplearning4j.earlystopping.EarlyStoppingResult;
import org.deeplearning4j.earlystopping.saver.InMemoryModelSaver;
import org.deeplearning4j.earlystopping.scorecalc.DataSetLossCalculator;
import org.deeplearning4j.earlystopping.termination.MaxEpochsTerminationCondition;
import org.deeplearning4j.earlystopping.termination.ScoreImprovementEpochTerminationCondition;
import org.deeplearning4j.earlystopping.trainer.EarlyStoppingTrainer;
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
public class LearnDigitsBackprop {

    /**
     * The main method.
     * @param args Not used.
     */
    public static void main(String[] args) {
        try {
            int seed = 43;
            double learningRate = 0.4;
            int nEpochs = 100;

            // Setup training data.
            System.out.println("Please wait, reading MNIST training data.");
            String dir = System.getProperty("user.dir");
            MNISTReader trainingReader = MNIST.loadMNIST(dir, true);
            MNISTReader validationReader = MNIST.loadMNIST(dir, false);

            DataSet trainingSet = trainingReader.getData();
            DataSet validationSet = validationReader.getData();

            DataSetIterator trainSetIterator = new ListDataSetIterator(trainingSet.asList(), trainingReader.getNumRows());
            DataSetIterator validationSetIterator = new ListDataSetIterator(validationSet.asList(), validationReader.getNumRows());

            System.out.println("Training set size: " + trainingReader.getNumImages());
            System.out.println("Validation set size: " + validationReader.getNumImages());

            System.out.println(trainingSet.get(0).getFeatures().size(1));
            System.out.println(validationSet.get(0).getFeatures().size(1));

            int numInputs = trainingReader.getNumCols()*trainingReader.getNumRows();
            int numOutputs = 10;
            int numHiddenNodes = 50;

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

            // Define when we want to stop training.
            EarlyStoppingModelSaver saver = new InMemoryModelSaver();
            EarlyStoppingConfiguration esConf = new EarlyStoppingConfiguration.Builder()
                    .epochTerminationConditions(new MaxEpochsTerminationCondition(500)) //Max of 50 epochs
                    .epochTerminationConditions(new ScoreImprovementEpochTerminationCondition(25))
                    .evaluateEveryNEpochs(1)
                    .scoreCalculator(new DataSetLossCalculator(validationSetIterator, true))     //Calculate test set score
                    .modelSaver(saver)
                    .build();
            EarlyStoppingTrainer trainer = new EarlyStoppingTrainer(esConf, conf, trainSetIterator);

            // Train and display result.
            EarlyStoppingResult result = trainer.fit();
            System.out.println("Termination reason: " + result.getTerminationReason());
            System.out.println("Termination details: " + result.getTerminationDetails());
            System.out.println("Total epochs: " + result.getTotalEpochs());
            System.out.println("Best epoch number: " + result.getBestModelEpoch());
            System.out.println("Score at best epoch: " + result.getBestModelScore());

            model = saver.getBestModel();
        } catch(Exception ex) {
            ex.printStackTrace();
        }

    }
}