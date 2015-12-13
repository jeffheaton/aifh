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
package com.heatonresearch.aifh.examples.neat;

import org.encog.Encog;
import org.encog.ml.CalculateScore;
import org.encog.ml.data.MLDataSet;
import org.encog.ml.data.basic.BasicMLDataSet;
import org.encog.ml.ea.train.EvolutionaryAlgorithm;
import org.encog.neural.neat.NEATNetwork;
import org.encog.neural.neat.NEATPopulation;
import org.encog.neural.neat.NEATUtil;
import org.encog.neural.networks.training.TrainingSetScore;
import org.encog.util.simple.EncogUtility;

/**
 * XOR-NEAT: This example solves the classic XOR operator neural
 * network problem.  However, it uses a NEAT evolving network.
 */
public class NEATXORExample {
    public static double XOR_INPUT[][] = { { 0.0, 0.0 }, { 1.0, 0.0 },
            { 0.0, 1.0 }, { 1.0, 1.0 } };

    public static double XOR_IDEAL[][] = { { 0.0 }, { 1.0 }, { 1.0 }, { 0.0 } };

    public static void main(final String args[]) {

        MLDataSet trainingSet = new BasicMLDataSet(XOR_INPUT, XOR_IDEAL);
        NEATPopulation pop = new NEATPopulation(2,1,1000);
        pop.setInitialConnectionDensity(1.0);// not required, but speeds training
        pop.reset();

        CalculateScore score = new TrainingSetScore(trainingSet);
        // train the neural network

        final EvolutionaryAlgorithm train = NEATUtil.constructNEATTrainer(pop,score);

        do {
            train.iteration();
            System.out.println("Epoch #" + train.getIteration() + " Error:" + train.getError()+ ", Species:" + pop.getSpecies().size());
        } while(train.getError() > 0.01);

        NEATNetwork network = (NEATNetwork)train.getCODEC().decode(train.getBestGenome());

        // test the neural network
        System.out.println("Neural Network Results:");
        EncogUtility.evaluate(network, trainingSet);

        Encog.getInstance().shutdown();
    }
}
