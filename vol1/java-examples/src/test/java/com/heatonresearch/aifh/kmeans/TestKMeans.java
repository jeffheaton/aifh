package com.heatonresearch.aifh.kmeans;

import com.heatonresearch.aifh.randomize.BasicGenerateRandom;
import junit.framework.TestCase;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 7/13/13
 * Time: 8:26 AM
 * To change this template use File | Settings | File Templates.
 */
public class TestKMeans extends TestCase {
    public static final double[][] DATA_SET = {
            {0, 0},
            {0, 1},
            {1, 0},
            {100, 100},
            {99, 100},
            {100, 99},
            {0, 100},
            {1, 100},
            {0, 99},
            {100, 0},
            {100, 1},
            {99, 0}
    };

    public void testClusterGeneral() {
        KMeans kmeans = new KMeans(4);
        kmeans.setRandomGeneration(new BasicGenerateRandom(22));
        kmeans.initForgy(DATA_SET);
        int iterations = kmeans.iteration(1000);
        assertEquals(3, iterations);

        Cluster cluster1 = kmeans.getClusters().get(0);
        Cluster cluster2 = kmeans.getClusters().get(1);
        Cluster cluster3 = kmeans.getClusters().get(2);
        Cluster cluster4 = kmeans.getClusters().get(3);

        assertEquals(3, cluster1.getObservations().size());
        assertEquals(3, cluster2.getObservations().size());
        assertEquals(3, cluster3.getObservations().size());
        assertEquals(3, cluster4.getObservations().size());


    }
}
