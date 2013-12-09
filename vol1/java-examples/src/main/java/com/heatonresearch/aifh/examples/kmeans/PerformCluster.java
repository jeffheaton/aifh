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

package com.heatonresearch.aifh.examples.kmeans;

import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.kmeans.Cluster;
import com.heatonresearch.aifh.kmeans.KMeans;
import com.heatonresearch.aifh.normalize.DataSet;

import java.io.InputStream;
import java.util.List;

/**
 * Try to cluster the Iris data set.
 */
public class PerformCluster {

    /**
     * The main method.
     *
     * @param args Not used.
     */
    public static void main(final String[] args) {
        final PerformCluster prg = new PerformCluster();
        prg.run();
    }

    /**
     * Perform the example.
     */
    public void run() {
        try {
            final InputStream istream = this.getClass().getResourceAsStream("/iris.csv");
            if( istream==null ) {
                System.out.println("Cannot access data set, make sure the resources are available.");
                System.exit(1);
            }

            final DataSet ds = DataSet.load(istream);
            istream.close();
            final List<BasicData> observations = ds.extractUnsupervisedLabeled(4);
            final KMeans kmeans = new KMeans(3);
            kmeans.initForgy(observations);
            final int iterations = kmeans.iteration(1000);
            System.out.println("Finished after " + iterations + " iterations.");

            for (int i = 0; i < kmeans.getK(); i++) {
                final Cluster cluster = kmeans.getClusters().get(i);
                System.out.println("* * * Cluster #" + i);
                for (final BasicData d : cluster.getObservations()) {
                    System.out.println(d.toString());
                }
            }
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }
}
