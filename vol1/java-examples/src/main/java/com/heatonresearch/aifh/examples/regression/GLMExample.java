/*
 * Artificial Intelligence for Humans
 * Volume 1: Fundamental Algorithms
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh

 * Copyright 2013 by Jeff Heaton
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

package com.heatonresearch.aifh.examples.regression;

import com.heatonresearch.aifh.examples.learning.SimpleLearn;
import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.general.fns.link.LogitLinkFunction;
import com.heatonresearch.aifh.normalize.DataSet;
import com.heatonresearch.aifh.regression.MultipleLinearRegression;
import com.heatonresearch.aifh.regression.TrainReweightLeastSquares;

import java.io.InputStream;
import java.util.List;

/**
 * Example that uses a GLM to predict the probability of breast cancer.
 */
public class GLMExample extends SimpleLearn {

    /**
     * Run the example.
     */
    public void process() {
        try {
            final InputStream istream = this.getClass().getResourceAsStream("/breast-cancer-wisconsin.csv");
            if( istream==null ) {
                System.out.println("Cannot access data set, make sure the resources are available.");
                System.exit(1);
            }
            final DataSet ds = DataSet.load(istream);
            istream.close();

            ds.deleteUnknowns();
            ds.deleteColumn(0);
            ds.replaceColumn(9, 4, 1, 0);
            final List<BasicData> trainingData = ds.extractSupervised(0, 9, 9, 1);

            final MultipleLinearRegression reg = new MultipleLinearRegression(9);
            reg.setLinkFunction(new LogitLinkFunction());
            final TrainReweightLeastSquares train = new TrainReweightLeastSquares(reg, trainingData);

            int iteration = 0;
            do {
                iteration++;
                train.iteration();
                System.out.println("Iteration #" + iteration + ", Error: " + train.getError());
            } while (iteration < 1000 && train.getError() > 0.01);

            query(reg, trainingData);
            System.out.println("Error: " + train.getError());


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
        final GLMExample prg = new GLMExample();
        prg.process();
    }
}
