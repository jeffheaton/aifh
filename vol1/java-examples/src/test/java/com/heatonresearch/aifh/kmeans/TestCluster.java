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

package com.heatonresearch.aifh.kmeans;

import com.heatonresearch.aifh.general.data.BasicData;
import org.junit.Test;

import static junit.framework.Assert.assertEquals;

/**
 * Test the cluster class.
 */
public class TestCluster {

    @Test
    public void testDimensions() {
        final Cluster cluster = new Cluster(3);
        assertEquals(true, cluster.toString().length() > 0);
        assertEquals(3, cluster.getDimensions());
    }

    @Test
    public void testCenter() {
        final Cluster cluster = new Cluster(3);
        final double[] ob1 = {2.0, 10.0, 100.0};
        final double[] ob2 = {4.0, 20.0, 200.0};
        final double[] ob3 = {6.0, 30.0, 300.0};

        cluster.getObservations().add(new BasicData(ob1));
        cluster.getObservations().add(new BasicData(ob2));
        cluster.getObservations().add(new BasicData(ob3));

        assertEquals(3, cluster.getObservations().size());

        cluster.calculateCenter();

        assertEquals(4.0, cluster.getCenter()[0], 0.00001);
        assertEquals(20.0, cluster.getCenter()[1], 0.00001);
        assertEquals(200.0, cluster.getCenter()[2], 0.00001);
    }
}
