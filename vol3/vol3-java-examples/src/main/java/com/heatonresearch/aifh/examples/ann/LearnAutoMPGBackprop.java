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
import com.heatonresearch.aifh.ann.activation.ActivationLinear;
import com.heatonresearch.aifh.ann.activation.ActivationReLU;
import com.heatonresearch.aifh.ann.train.BackPropagation;
import com.heatonresearch.aifh.error.ErrorCalculationMSE;
import com.heatonresearch.aifh.examples.learning.SimpleLearn;
import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.general.data.DataUtil;
import com.heatonresearch.aifh.normalize.DataSet;

import java.io.InputStream;
import java.util.List;

/**
 *
 */
public class LearnAutoMPGBackprop extends SimpleLearn {
    /**
     * Run the example.
     */
    public void process() {
        try {
            final InputStream istream = this.getClass().getResourceAsStream("/auto-mpg.data.csv");
            if( istream==null ) {
                System.out.println("Cannot access data set, make sure the resources are available.");
                System.exit(1);
            }
            final DataSet ds = DataSet.load(istream);
            istream.close();

            // The following ranges are setup for the Auto MPG data set.  If you wish to normalize other files you will
            // need to modify the below function calls other files.

            // First remove some columns that we will not use:
            ds.deleteColumn(8); // Car name
            ds.deleteColumn(7); // Car origin
            ds.deleteColumn(6); // Year
            ds.deleteUnknowns();

            ds.normalizeZScore(1);
            ds.normalizeZScore(2);
            ds.normalizeZScore(3);
            ds.normalizeZScore(4);
            ds.normalizeZScore(5);

            List<BasicData> trainingData = ds.extractSupervised(1, 4, 0, 1);

            List<List<BasicData>> splitList = DataUtil.split(trainingData,0.75);
            trainingData = splitList.get(0);
            List<BasicData> validationData = splitList.get(1);

            System.out.println("Size of dataset: " + ds.size());
            System.out.println("Size of training set: " + trainingData.size());
            System.out.println("Size of validation set: " + validationData.size());

            int inputCount = trainingData.get(0).getInput().length;

            BasicNetwork network = new BasicNetwork();
            network.addLayer(new BasicLayer(null,true,inputCount));
            network.addLayer(new BasicLayer(new ActivationReLU(),true,50));
            network.addLayer(new BasicLayer(new ActivationReLU(),true,25));
            network.addLayer(new BasicLayer(new ActivationReLU(),true,5));
            network.addLayer(new BasicLayer(new ActivationLinear(),false,1));
            network.finalizeStructure();
            network.reset();

            final BackPropagation train = new BackPropagation(network, trainingData, 0.000001, 0.9);

            performIterationsEarlyStop(train, network, validationData, 20, new ErrorCalculationMSE());
            query(network, validationData);

        } catch (Throwable t) {
            t.printStackTrace();
        }
    }

    /**
     * The main method.
     *
     * @param args Not used.
     */
    public static void main(final String[] args) {
        final LearnAutoMPGBackprop prg = new LearnAutoMPGBackprop();
        prg.process();
    }
}

