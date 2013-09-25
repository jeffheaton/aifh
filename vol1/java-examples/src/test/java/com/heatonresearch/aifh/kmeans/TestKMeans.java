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

import com.heatonresearch.aifh.AIFHError;
import com.heatonresearch.aifh.distance.EuclideanDistance;
import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.randomize.BasicGenerateRandom;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.assertEquals;

/**
 * Test KMeans.
 */
public class TestKMeans {

    public static List<BasicData> getDataSet() {
        final List<BasicData> result = new ArrayList<BasicData>();
        result.add(new BasicData(new double[]{0, 0}, "a"));
        result.add(new BasicData(new double[]{0, 1}, "a"));
        result.add(new BasicData(new double[]{1, 0}, "a"));
        result.add(new BasicData(new double[]{100, 100}, "b"));
        result.add(new BasicData(new double[]{99, 100}, "b"));
        result.add(new BasicData(new double[]{100, 99}, "b"));
        result.add(new BasicData(new double[]{0, 100}, "c"));
        result.add(new BasicData(new double[]{1, 100}, "c"));
        result.add(new BasicData(new double[]{0, 99}, "c"));
        result.add(new BasicData(new double[]{100, 0}, "d"));
        result.add(new BasicData(new double[]{100, 1}, "d"));
        result.add(new BasicData(new double[]{99, 0}, "d"));


        return result;
    }

    @Test
    public void testClusterForgy() {
        final KMeans kmeans = new KMeans(4);
        kmeans.setRandomGeneration(new BasicGenerateRandom(22));
        kmeans.initForgy(getDataSet());
        final int iterations = kmeans.iteration(1000);
        assertEquals(3, iterations);

        final Cluster cluster1 = kmeans.getClusters().get(0);
        final Cluster cluster2 = kmeans.getClusters().get(1);
        final Cluster cluster3 = kmeans.getClusters().get(2);
        final Cluster cluster4 = kmeans.getClusters().get(3);

        assertEquals(3, cluster1.getObservations().size());
        assertEquals(3, cluster2.getObservations().size());
        assertEquals(3, cluster3.getObservations().size());
        assertEquals(3, cluster4.getObservations().size());
    }

    @Test
    public void testClusterRandom() {
        final KMeans kmeans = new KMeans(4);
        kmeans.setRandomGeneration(new BasicGenerateRandom(22));
        kmeans.initRandom(getDataSet());
        final int iterations = kmeans.iteration(1000);
        assertEquals(4, iterations);

        final Cluster cluster1 = kmeans.getClusters().get(0);
        final Cluster cluster2 = kmeans.getClusters().get(1);
        final Cluster cluster3 = kmeans.getClusters().get(2);
        final Cluster cluster4 = kmeans.getClusters().get(3);

        assertEquals(3, cluster1.getObservations().size());
        assertEquals(3, cluster2.getObservations().size());
        assertEquals(3, cluster3.getObservations().size());
        assertEquals(3, cluster4.getObservations().size());
    }

    @Test
    public void testGeneral() {
        final KMeans kmeans = new KMeans(5);
        assertEquals(5, kmeans.getK());
        kmeans.setRandomGeneration(new BasicGenerateRandom());
        kmeans.setDistanceMetric(new EuclideanDistance());
        assertEquals(true, kmeans.getRandomGeneration() instanceof BasicGenerateRandom);
        assertEquals(true, kmeans.getDistanceMetric() instanceof EuclideanDistance);
    }

    @Test(expected = AIFHError.class)
    public void testTooManyClusters() {
        final KMeans kmeans = new KMeans(13);
        kmeans.initRandom(getDataSet());
    }

    @Test(expected = AIFHError.class)
    public void testEarlyIteration() {
        final KMeans kmeans = new KMeans(3);
        kmeans.iteration();
    }

    @Test(expected = AIFHError.class)
    public void testNoObservations() {
        final List<BasicData> list = new ArrayList<BasicData>();
        final KMeans kmeans = new KMeans(3);
        kmeans.initForgy(list);
    }

    @Test(expected = AIFHError.class)
    public void testNoDimension() {
        final List<BasicData> list = new ArrayList<BasicData>();
        list.add(new BasicData(0));
        final KMeans kmeans = new KMeans(3);
        kmeans.initForgy(list);
    }

    @Test
    public void testMaxClusters() {
        final KMeans kmeans = new KMeans(12);
        kmeans.setRandomGeneration(new BasicGenerateRandom(22));
        kmeans.initRandom(getDataSet());
        final int iterations = kmeans.iteration(1000);
        assertEquals(1, iterations);
    }
}
