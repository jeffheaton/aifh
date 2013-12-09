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
import com.heatonresearch.aifh.normalize.DataSet;
import com.heatonresearch.aifh.regression.MultipleLinearRegression;
import com.heatonresearch.aifh.regression.TrainLeastSquares;

import java.io.InputStream;
import java.util.Arrays;
import java.util.List;

/**
 * Linear regression example.
 */
public class LinearRegressionExample extends SimpleLearn {
    public void process() {
        try {
            final InputStream istream = this.getClass().getResourceAsStream("/abalone.csv");
            if( istream==null ) {
                System.out.println("Cannot access data set, make sure the resources are available.");
                System.exit(1);
            }
            final DataSet ds = DataSet.load(istream);
            // The following ranges are setup for the Abalone data set.  If you wish to normalize other files you will
            // need to modify the below function calls other files.
            ds.encodeOneOfN(0, 0, 1);
            istream.close();

            final List<BasicData> trainingData = ds.extractSupervised(0, 10, 10, 1);

            final MultipleLinearRegression reg = new MultipleLinearRegression(10);
            final TrainLeastSquares train = new TrainLeastSquares(reg, trainingData);
            train.iteration();
            System.out.println(Arrays.toString(reg.getLongTermMemory()));
            query(reg, trainingData);
            System.out.println("Error: " + train.getError());


        } catch (Throwable t) {
            t.printStackTrace();
        }


    }

    public static void main(final String[] args) {
        final LinearRegressionExample prg = new LinearRegressionExample();
        prg.process();
    }
}
