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
import com.heatonresearch.aifh.ann.activation.ActivationReLU;
import com.heatonresearch.aifh.ann.activation.ActivationSoftMax;
import com.heatonresearch.aifh.ann.train.BackPropagation;
import com.heatonresearch.aifh.examples.learning.SimpleLearn;
import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.normalize.DataSet;

import java.io.InputStream;
import java.util.List;
import java.util.Map;

/**
 * This example shows how to create a simple classification neural network for the Iris dataset.
 * An input layer with 4 neurons is used for the 4 input measurements.  A dense (BasicLayer) ReLU layer
 * is used for the hidden and a softmax on the output.  Because this is a classification problem,
 * a Softmax is used for the output.  This causes the 3 outputs to specify the relative probability
 * of the iris measurements being one of the 3 output species.
 *
 * The input data are normalized to the range [-1,1].
 */
public class LearnIrisBackprop extends SimpleLearn {
    /**
     * Run the example.
     */
    public void process() {
        try {
            final InputStream istream = this.getClass().getResourceAsStream("/iris.csv");
            if( istream==null ) {
                System.out.println("Cannot access data set, make sure the resources are available.");
                System.exit(1);
            }
            final DataSet ds = DataSet.load(istream);
            // The following ranges are setup for the Iris data set.  If you wish to normalize other files you will
            // need to modify the below function calls other files.
            ds.normalizeRange(0, -1, 1);
            ds.normalizeRange(1, -1, 1);
            ds.normalizeRange(2, -1, 1);
            ds.normalizeRange(3, -1, 1);
            final Map<String, Integer> species = ds.encodeOneOfN(4); // species is column 4
            istream.close();

            final List<BasicData> trainingData = ds.extractSupervised(0, 4, 4, 3);

            BasicNetwork network = new BasicNetwork();
            network.addLayer(new BasicLayer(null,true,4));
            network.addLayer(new BasicLayer(new ActivationReLU(),true,20));
            network.addLayer(new BasicLayer(new ActivationSoftMax(),false,3));
            network.finalizeStructure();
            network.reset();

            final BackPropagation train = new BackPropagation(network, trainingData, 0.001, 0.9);

            performIterations(train, 100000, 0.02, true);
            queryOneOfN(network, trainingData, species);

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
        final LearnIrisBackprop prg = new LearnIrisBackprop();
        prg.process();
    }
}

