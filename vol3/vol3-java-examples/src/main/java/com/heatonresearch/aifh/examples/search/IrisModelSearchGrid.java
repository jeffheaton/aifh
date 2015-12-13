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
package com.heatonresearch.aifh.examples.search;

import com.heatonresearch.aifh.ann.BasicLayer;
import com.heatonresearch.aifh.ann.BasicNetwork;
import com.heatonresearch.aifh.ann.activation.ActivationFunction;
import com.heatonresearch.aifh.ann.activation.ActivationReLU;
import com.heatonresearch.aifh.ann.activation.ActivationSoftMax;
import com.heatonresearch.aifh.ann.activation.ActivationTANH;
import com.heatonresearch.aifh.ann.train.BackPropagation;
import com.heatonresearch.aifh.examples.learning.SimpleLearn;
import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.general.data.DataUtil;
import com.heatonresearch.aifh.normalize.DataSet;
import com.heatonresearch.aifh.selection.GridModelSelection;

import java.io.IOError;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.List;
import java.util.Map;


public class IrisModelSearchGrid {
    public static final int RUN_CYCLES = 5;
    public static final double LEARNING_RATE = 0.00001;
    public static final double MAX_EPOCHS = 5000;
    private ModelSearchResults globalBest;

    public List<BasicData> normalizeDataset() throws IOException {
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

        return ds.extractSupervised(0, 4, 4, 3);
    }

    public ModelSearchResults performTrainingRun(List<BasicData> trainingData, List<BasicData> validationData, Object[] hyperParams) {
        ActivationFunction af;
        if( hyperParams[0].equals("relu")) {
            af = new ActivationReLU();
        } else {
            af = new ActivationTANH();
        }

        int h1 = (int)(double)hyperParams[1];
        int h2 = (int)(double)hyperParams[2];

        BasicNetwork network = new BasicNetwork();
        network.addLayer(new BasicLayer(null,true,4));
        network.addLayer(new BasicLayer(af,true,h1));
        if (h2>0) {
            network.addLayer(new BasicLayer(af, true, h2));
        }
        network.addLayer(new BasicLayer(new ActivationSoftMax(),false,3));
        network.finalizeStructure();
        network.reset();

        int badEpochs = 0;
        int epochs = 0;
        double bestError = Double.POSITIVE_INFINITY;
        final BackPropagation train = new BackPropagation(network, trainingData, LEARNING_RATE, 0.9);
        while(badEpochs<10 && epochs<MAX_EPOCHS) {
            train.iteration();
            epochs++;
            double error = train.getLastError();
            if( error<bestError ) {
                bestError = error;
                badEpochs = 0;
            } else {
                badEpochs++;
            }
        }
        return new ModelSearchResults(epochs,bestError,hyperParams);
    }

    public void evaluate(List<BasicData> trainingData, List<BasicData> validationData, Object[] hyperParams) {
        ModelSearchResults bestRun = null;
        for(int i=0;i<RUN_CYCLES;i++) {
            ModelSearchResults run = performTrainingRun(trainingData, validationData, hyperParams);
            System.out.println(run);
            if( bestRun==null || bestRun.compareTo(run)>0) {
                bestRun = run;
            }
        }

        System.out.println(bestRun.toString());

        if(this.globalBest ==null || this.globalBest.compareTo(bestRun)>0 ) {
            this.globalBest = bestRun;
        }
    }

    /**
     * Run the example.
     */
    public void process() {
        try {
            List<BasicData> dataset = normalizeDataset();
            List<List<BasicData>> splitSet = DataUtil.split(dataset,0.75);
            List<BasicData> trainingData = splitSet.get(0);
            List<BasicData> validationData = splitSet.get(1);

            System.out.println("Training dataset size: " + trainingData.size());
            System.out.println("Validation dataset size: " + validationData.size());

            GridModelSelection grid = new GridModelSelection();
            grid.addCategoryAxis(new String[] {"relu", "tanh"});
            grid.addNumericAxis(1,10,1);
            grid.addNumericAxis(0,5,1);
            Object[] hyperParams;

            while( (hyperParams=grid.next()) != null ) {
                evaluate(trainingData,validationData,hyperParams);
            }

            //Object[] line = {"relu",new Double(10),new Double(0)};
            //evaluate(trainingData,validationData,line);


            System.out.println();
            System.out.println("Best: " + this.globalBest);


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
        final IrisModelSearchGrid prg = new IrisModelSearchGrid();
        prg.process();
    }
}

