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
import org.deeplearning4j.eval.Evaluation;
import org.deeplearning4j.nn.api.OptimizationAlgorithm;
import org.deeplearning4j.nn.conf.GradientNormalization;
import org.deeplearning4j.nn.conf.MultiLayerConfiguration;
import org.deeplearning4j.nn.conf.NeuralNetConfiguration;
import org.deeplearning4j.nn.conf.Updater;
import org.deeplearning4j.nn.conf.layers.*;
import org.deeplearning4j.nn.conf.layers.setup.ConvolutionLayerSetup;
import org.deeplearning4j.nn.multilayer.MultiLayerNetwork;
import org.deeplearning4j.nn.weights.WeightInit;
import org.deeplearning4j.optimize.listeners.ScoreIterationListener;
import org.nd4j.linalg.api.ndarray.INDArray;
import org.nd4j.linalg.dataset.DataSet;
import org.nd4j.linalg.factory.Nd4j;
import org.nd4j.linalg.lossfunctions.LossFunctions;
import org.nd4j.linalg.lossfunctions.LossFunctions.LossFunction;

/**
 * A convolutional neural network is used to predict the MNIST digits.  This neural network makes use of the
 * Deeplearning4J framework. This neural network trains to an accuracy of around 98%.  The convolutional neural
 * network might take hours to train, depending on your hardware.
 */
public class LearnDigitsConv {

    /**
     * The main method.
     * @param args Not used.
     */
    public static void main(String[] args) {
        try {
            int seed = 43;
            double learningRate = 1e-2;
            int nEpochs = 50;
            int batchSize = 500;
            int channels = 1;

            // Setup training data.
            System.out.println("Please wait, reading MNIST training data.");
            String dir = System.getProperty("user.dir");
            MNISTReader trainingReader = MNIST.loadMNIST(dir, true);
            MNISTReader validationReader = MNIST.loadMNIST(dir, false);

            DataSet trainingSet = trainingReader.getData();
            DataSet validationSet = validationReader.getData();

            DataSetIterator trainSetIterator = new ListDataSetIterator(trainingSet.asList(), batchSize);
            DataSetIterator validationSetIterator = new ListDataSetIterator(validationSet.asList(), validationReader.getNumRows());

            System.out.println("Training set size: " + trainingReader.getNumImages());
            System.out.println("Validation set size: " + validationReader.getNumImages());

            int numOutputs = 10;

            // Create neural network.
            MultiLayerConfiguration.Builder builder = new NeuralNetConfiguration.Builder()
                    .seed(seed)
                    .iterations(1)
                    .regularization(true).l2(0.0005)
                    .learningRate(0.01)
                    .weightInit(WeightInit.XAVIER)
                    .optimizationAlgo(OptimizationAlgorithm.STOCHASTIC_GRADIENT_DESCENT)
                    .updater(Updater.NESTEROVS).momentum(0.9)
                    .list(4)
                    .layer(0, new ConvolutionLayer.Builder(5, 5)
                            .nIn(channels)
                            .stride(1, 1)
                            .nOut(20).dropOut(0.5)
                            .activation("relu")
                            .build())
                    .layer(1, new SubsamplingLayer.Builder(SubsamplingLayer.PoolingType.MAX)
                            .kernelSize(2,2)
                            .stride(2,2)
                            .build())
                    .layer(2, new DenseLayer.Builder().activation("relu")
                            .nOut(500).build())
                    .layer(3, new OutputLayer.Builder(LossFunctions.LossFunction.NEGATIVELOGLIKELIHOOD)
                            .nOut(10)
                            .activation("softmax")
                            .build())
                    .backprop(true).pretrain(false);

            new ConvolutionLayerSetup(builder,28,28,1);
            MultiLayerConfiguration conf = builder.build();


            MultiLayerNetwork model = new MultiLayerNetwork(conf);
            model.init();
            model.setListeners(new ScoreIterationListener(1));

            // Define when we want to stop training.
            EarlyStoppingModelSaver saver = new InMemoryModelSaver();
            EarlyStoppingConfiguration esConf = new EarlyStoppingConfiguration.Builder()
                    //.epochTerminationConditions(new MaxEpochsTerminationCondition(10))
                    .epochTerminationConditions(new ScoreImprovementEpochTerminationCondition(5))
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

            // Evaluate
            Evaluation eval = new Evaluation(numOutputs);
            validationSetIterator.reset();

            for (int i = 0; i < validationSet.numExamples(); i++) {
                DataSet t = validationSet.get(i);
                INDArray features = t.getFeatureMatrix();
                INDArray labels = t.getLabels();
                INDArray predicted = model.output(features, false);
                eval.eval(labels, predicted);
            }

            //Print the evaluation statistics
            System.out.println(eval.stats());
        } catch(Exception ex) {
            ex.printStackTrace();
        }

    }
}