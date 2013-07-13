package com.heatonresearch.aifh.kmeans;

import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.general.data.UnsupervisedData;
import com.heatonresearch.aifh.randomize.BasicGenerateRandom;
import junit.framework.TestCase;

import java.util.ArrayList;
import java.util.List;

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

    public static List<UnsupervisedData> getDataSet() {
        List<UnsupervisedData> result = new ArrayList<UnsupervisedData>();
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

    public void testClusterGeneral() {
        KMeans kmeans = new KMeans(4);
        kmeans.setRandomGeneration(new BasicGenerateRandom(22));
        kmeans.initForgy(getDataSet());
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
