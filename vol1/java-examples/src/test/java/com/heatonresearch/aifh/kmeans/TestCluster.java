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
        Cluster cluster = new Cluster(3);
        assertEquals(true, cluster.toString().length() > 0);
        assertEquals(3, cluster.getDimensions());
    }

    @Test
    public void testCenter() {
        Cluster cluster = new Cluster(3);
        double[] ob1 = {2.0, 10.0, 100.0};
        double[] ob2 = {4.0, 20.0, 200.0};
        double[] ob3 = {6.0, 30.0, 300.0};

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
