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
package com.heatonresearch.aifh.examples.dbn;

import com.heatonresearch.aifh.dbnn.DeepBeliefNetwork;
import com.heatonresearch.aifh.dbnn.SupervisedTrainDBN;
import com.heatonresearch.aifh.dbnn.UnsupervisedTrainDBN;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

import java.util.Arrays;

/**
 * This example trains a deep belief neural network.  The training begins with unsupervised pretraining,
 * followed by supervised training of the logisitic regression output layer.
 */
public class SimpleDBN {
    public static final double LearningRateUnsupervised = 0.1;
    public static final double LearningRateSupervised = 0.1;
    public static final int K = 1;

    // training data
    public static final double[][] TRAINING_INPUT = {
            {1, 1, 1, 1, 0, 0, 0, 0},
            {1, 1, 0, 1, 0, 0, 0, 0},
            {1, 1, 1, 0, 0, 0, 0, 0},
            {0, 0, 0, 0, 1, 1, 1, 1},
            {0, 0, 0, 0, 1, 1, 0, 1},
            {0, 0, 0, 0, 1, 1, 1, 0}
    };

    public static final double[][] TRAINING_IDEAL = {
            {1, 0},
            {1, 0},
            {1, 0},
            {0, 1},
            {0, 1},
            {0, 1},
    };

    public static final double[][] TEST_INPUT = {
            {0, 1, 1, 1, 0, 0, 0, 0},
            {1, 0, 1, 1, 0, 0, 0, 0},
            {0, 0, 0, 0, 0, 1, 1, 1},
            {0, 0, 0, 0, 1, 0, 1, 1},
    };


    public static void main(String[] args) {

        // Create an dbnn belief network.
        int[] hidden = {2, 3};
        DeepBeliefNetwork dbn = new DeepBeliefNetwork(TRAINING_INPUT[0].length, hidden, TRAINING_IDEAL[0].length);
        dbn.setRandom(new MersenneTwisterGenerateRandom(54321));
        dbn.reset();


        // Layer by layer unsupervised training.
        for(int level=0;level<hidden.length;level++) {
            UnsupervisedTrainDBN trainUnsupervised = new UnsupervisedTrainDBN(
                    dbn,level,TRAINING_INPUT,LearningRateUnsupervised,K);
            for(int i=0;i<2000;i++) {
                trainUnsupervised.iteration();
            }
        }

        // Supervised training.
        SupervisedTrainDBN trainSupervised = new SupervisedTrainDBN(
                dbn,TRAINING_INPUT,TRAINING_IDEAL,LearningRateSupervised);
        int iteration = 0;
        do {
            iteration++;
            trainSupervised.iteration();
            System.out.println("Iteration: " + iteration + ", Supervised training: error = "
                    + trainSupervised.getLastError());
        } while(trainSupervised.getLastError()>0.001);


        // Use test data.
        for(double[] input : TEST_INPUT) {
            //double[] output = dbn.computeRegression(input);
            //System.out.println(Arrays.toString(input) + " -> " + Arrays.toString(output));
        }
    }
}
