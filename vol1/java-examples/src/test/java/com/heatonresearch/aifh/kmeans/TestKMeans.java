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
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 7/13/13
 * Time: 8:26 AM
 * To change this template use File | Settings | File Templates.
 */
public class TestKMeans {

    public static List<BasicData> getDataSet() {
        List<BasicData> result = new ArrayList<BasicData>();
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

    @Test
    public void testClusterRandom() {
        KMeans kmeans = new KMeans(4);
        kmeans.setRandomGeneration(new BasicGenerateRandom(22));
        kmeans.initRandom(getDataSet());
        int iterations = kmeans.iteration(1000);
        assertEquals(4, iterations);

        Cluster cluster1 = kmeans.getClusters().get(0);
        Cluster cluster2 = kmeans.getClusters().get(1);
        Cluster cluster3 = kmeans.getClusters().get(2);
        Cluster cluster4 = kmeans.getClusters().get(3);

        assertEquals(3, cluster1.getObservations().size());
        assertEquals(3, cluster2.getObservations().size());
        assertEquals(3, cluster3.getObservations().size());
        assertEquals(3, cluster4.getObservations().size());
    }

    @Test
    public void testGeneral() {
        KMeans kmeans = new KMeans(5);
        assertEquals(5, kmeans.getK());
        kmeans.setRandomGeneration(new BasicGenerateRandom());
        kmeans.setDistanceMetric(new EuclideanDistance());
        assertEquals(true, kmeans.getRandomGeneration() instanceof BasicGenerateRandom);
        assertEquals(true, kmeans.getDistanceMetric() instanceof EuclideanDistance);
    }

    @Test(expected = AIFHError.class)
    public void testTooManyClusters() {
        KMeans kmeans = new KMeans(13);
        kmeans.initRandom(getDataSet());
    }

    @Test(expected = AIFHError.class)
    public void testEarlyIteration() {
        KMeans kmeans = new KMeans(3);
        kmeans.iteration();
    }

    @Test(expected = AIFHError.class)
    public void testNoObservations() {
        List<BasicData> list = new ArrayList<BasicData>();
        KMeans kmeans = new KMeans(3);
        kmeans.initForgy(list);
    }

    @Test(expected = AIFHError.class)
    public void testNoDimension() {
        List<BasicData> list = new ArrayList<BasicData>();
        list.add(new BasicData(0));
        KMeans kmeans = new KMeans(3);
        kmeans.initForgy(list);
    }

    @Test
    public void testMaxClusters() {
        KMeans kmeans = new KMeans(12);
        kmeans.setRandomGeneration(new BasicGenerateRandom(22));
        kmeans.initRandom(getDataSet());
        int iterations = kmeans.iteration(1000);
        assertEquals(1, iterations);
    }
}
